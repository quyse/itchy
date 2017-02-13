{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, TupleSections, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Main(main) where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import Magic
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import qualified System.Directory as D
import qualified System.Environment as E
import System.Exit
import System.IO
import System.IO.Unsafe
import qualified System.Process as P
import qualified System.Posix as P
import qualified Text.Toml as Toml

import Flaw.Book

import Itchy.Itch
import Itchy.Report

{-# NOINLINE reportRef #-}
reportRef :: IORef Report
reportRef = unsafePerformIO $ newIORef Report
	{ report_error = Nothing
	, report_download = ReportDownload_notStarted
	, report_avCheck = ReportAVCheck_notStarted
	, report_unpack = ReportUnpack_notStarted
	}

report :: (Report -> Report) -> IO ()
report = modifyIORef' reportRef

reportError :: (Report -> Report) -> IO a
reportError f = do
	report f
	throwIO ReportedError

handleReport :: (T.Text -> Report -> Report) -> IO a -> IO a
handleReport h io = io `catches`
	[ Handler $ \ReportedError -> throwIO ReportedError
	, Handler $ \(SomeException e) -> reportError $ h $ T.pack $ show e
	]

handleIsolatedReport :: (T.Text -> Report -> Report) -> IO () -> IO ()
handleIsolatedReport h io = io `catches`
	[ Handler $ \ReportedError -> return ()
	, Handler $ \(SomeException e) -> reportError $ h $ T.pack $ show e
	]

data ReportedError = ReportedError deriving Show
instance Exception ReportedError

main :: IO ()
main = do
	run `catches`
		[ Handler $ \ReportedError -> return ()
		, Handler $ \(SomeException e) -> report $ \r -> r
			{ report_error = Just $ T.pack $ show e
			}
		]
	T.putStrLn . T.decodeUtf8 . Y.encode =<< readIORef reportRef

run :: IO ()
run = withBook $ \bk -> do
	uploadFileName <- E.getEnv "ITCHIO_UPLOAD_FILENAME"

	-- download if needed
	maybeUploadId <- (read <$>) <$> E.lookupEnv "ITCHIO_UPLOAD_ID"
	case maybeUploadId of
		Just uploadId -> handleReport (\e r -> r
			{ report_download = ReportDownload_failed e
			}) $ do
			-- get http manager
			httpManager <- H.getGlobalManager

			-- init itch api
			itchToken <- T.pack <$> E.getEnv "ITCHIO_API_KEY"
			itchApi <- book bk $ newItchApi httpManager itchToken

			-- try to get download key id
			maybeDownloadKeyId <- (read <$>) <$> E.lookupEnv "ITCHIO_DOWNLOAD_KEY_ID"

			-- get download url
			url <- itchDownloadUpload itchApi uploadId maybeDownloadKeyId

			-- download
			request <- H.parseUrlThrow $ T.unpack url
			withFile uploadFileName WriteMode $ \h -> do
				H.withResponse request httpManager $ \(H.responseBody -> bodyReader) -> let
					step = do
						chunk <- H.brRead bodyReader
						unless (B.null chunk) $ do
							B.hPut h chunk
							step
					in step

			report $ \r -> r
				{ report_download = ReportDownload_succeeded
				}
		Nothing -> report $ \r -> r
			{ report_download = ReportDownload_skipped
			}

	-- AV check
	handleReport (\e r -> r
		{ report_avCheck = ReportAVCheck_failed e
		}) $ do
		(exitCode, output, errOutput) <- P.readProcessWithExitCode "clamscan" ["-ria", "--no-summary", uploadFileName] ""
		case exitCode of
			ExitSuccess -> report $ \r -> r
				{ report_avCheck = ReportAVCheck_ok
				}
			ExitFailure _ -> reportError $ \r -> r
				{ report_avCheck = ReportAVCheck_failed $ T.pack output <> T.pack errOutput
				}

	-- initialize libmagic
	magic <- magicOpen [MagicMimeType]
	magicLoadDefault magic

	-- unpack
	handleReport (\e r -> r
		{ report_unpack = ReportUnpack_failed e
		}) $ do
		P.callProcess "unar" ["-q", "-o", WORK_DIR, uploadFileName]

		-- parse entries
		let
			parseEntry name path = do
				status <- P.getSymbolicLinkStatus $ T.unpack path
				let P.CMode mode = P.fileMode status
				if P.isRegularFile status then do
					parses <- parseFile magic name path
					return ReportEntry_file
						{ reportEntry_mode = mode
						, reportEntry_size = fromIntegral $ P.fileSize status
						, reportEntry_parses = parses
						}
				else if P.isDirectory status then do
					subEntries <- parseSubEntries path
					return ReportEntry_directory
						{ reportEntry_mode = mode
						, reportEntry_entries = subEntries
						}
				else if P.isSymbolicLink status then do
					link <- P.readSymbolicLink $ T.unpack path
					return ReportEntry_symlink
						{ reportEntry_mode = mode
						, reportEntry_link = T.pack link
						}
				else return ReportEntry_unknown
					{ reportEntry_mode = mode
					}
			parseSubEntries path = do
				subNames <- D.listDirectory $ T.unpack path
				HM.fromList <$> forM subNames (\(T.pack -> subName) -> (subName, ) <$> parseEntry subName (path <> "/" <> subName))

		entries <- parseSubEntries WORK_DIR

		report $ \r -> r
			{ report_unpack = ReportUnpack_succeeded entries
			}

parseFile :: Magic -> T.Text -> T.Text -> IO [ReportParse]
parseFile magic name path = do
	-- get type of file with libmagic
	mime <- magicFile magic $ T.unpack path

	parsesRef <- newIORef []
	let addParse parse = modifyIORef' parsesRef (parse :)

	-- ELF (Linux)
	when (mime == "application/x-executable" || mime == "application/x-sharedlib") $ do
		-- get some info from ELF header
		elfHeader <- withFile (T.unpack path) ReadMode $ \h -> B.hGet h 0x14
		let arch = either (const ReportArch_unknown) id $ flip S.runGet elfHeader $ do
			elfMagic <- S.getBytes 4
			unless (elfMagic == "\x7F\x45\x4C\x46") $ fail "wrong ELF magic"
			elfClass <- S.getWord8
			unless (elfClass == 1 || elfClass == 2) $ fail "wrong ELF class"
			elfEndianness <- S.getWord8
			unless (elfEndianness == 1 || elfEndianness == 2) $ fail "wrong ELF endianness"
			elfVersion <- S.getWord8
			unless (elfVersion == 1) $ fail "wrong ELF version"
			_elfAbi <- S.getWord8
			_elfAbiVersion <- S.getWord8
			S.skip 7
			let getWord16 = if elfEndianness == 1 then S.getWord16le else S.getWord16be
			_elfType <- getWord16
			elfMachine <- getWord16
			return $ case elfMachine of
				0x03 -> ReportArch_x86
				0x3E -> ReportArch_x64
				_ -> ReportArch_unknown
		-- get ELF libraries the executable depends on
		{-
		I run the following:
		find /usr/bin -exec readelf -d {} \; | grep NEEDED
		and checked that the following regexp matches all 12744 lines:
		^ 0x[0-9]+ \(NEEDED\) +Shared library: \[[a-zA-Z0-9\-\.\+\_]+\]$
		So must be pretty solid.
		-}
		readelfOutput <- T.pack <$> P.readProcess "readelf" ["-rd", T.unpack path] ""
		-- get needed libraries
		let neededLibraries = concat $ flip map (T.lines readelfOutput) $ \case
			(T.words -> [_, "(NEEDED)", "Shared", "library:", libraryWithBrackets]) ->
				if T.length libraryWithBrackets > 2
					&& T.head libraryWithBrackets == '['
					&& T.last libraryWithBrackets == ']' then
					[T.take (T.length libraryWithBrackets - 2) $ T.drop 1 libraryWithBrackets]
				else []
			_ -> []
		let neededDeps = flip map neededLibraries $ \library -> ReportDep
			{ reportDep_name = library
			, reportDep_version = T.empty
			}
		-- calculate glibc version
		let glibcVersions = concat $ flip map (T.lines readelfOutput) $ \case
			(T.words -> (_ : _ : _ : _ : (T.breakOn "@GLIBC_" -> (_, glibcVersion)) : _)) | T.length glibcVersion > 0 -> [T.drop 7 glibcVersion]
			_ -> []
		let deps = if null glibcVersions then neededDeps
			else ReportDep
				{ reportDep_name = "GLIBC"
				, reportDep_version = T.intercalate "." $ map (T.pack . show)
					(maximum $ map (map (read . T.unpack) . T.splitOn ".") glibcVersions :: [Int])
				} : neededDeps
		addParse $ ReportParse_binaryElf ReportBinaryElf
			{ reportBinaryElf_arch = arch
			, reportBinaryElf_deps = deps
			}

	-- EXE (Windows)
	-- when (mime == "application/x-dosexec")

	-- Mach-O (macOS)
	-- when (mime == "application/x-mach-binary")

	-- parse .itch.toml
	when (name == ".itch.toml") $ addParse =<< let
		f = ReportParse_itchToml . \case
			Right (Right tomlDoc) -> case A.fromJSON $ A.toJSON tomlDoc of
				A.Success itchToml -> Right itchToml
				A.Error itchTomlErr -> Left $ T.pack itchTomlErr
			Right (Left tomlErr) -> Left $ T.pack $ show tomlErr
			Left (SomeException e) -> Left $ T.pack $ show e
		in f <$> try (Toml.parseTomlDoc ".itch.toml" <$> T.readFile (T.unpack path))

	readIORef parsesRef

pattern WORK_DIR = "package"
