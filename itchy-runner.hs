{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections, ViewPatterns #-}

module Main(main) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.IORef
import Data.Maybe
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import qualified Data.Yaml as Y
import qualified GHC.IO.Encoding
import Magic
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import qualified Options.Applicative as O
import qualified System.Directory as D
import qualified System.Environment as E
import System.Exit
import System.FilePath(takeDirectory)
import System.IO
import System.IO.Temp(withTempDirectory)
import System.IO.Unsafe
import qualified System.Process as P
import qualified System.Posix as P
import qualified Text.Toml as Toml

import Flaw.Book
import Flaw.Flow

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

ignoreErrors :: IO () -> IO ()
ignoreErrors = handle $ \SomeException {} -> return ()

handleTimeout :: Int -> IO a -> IO a
handleTimeout timeout io = do
	delayVar <- registerDelay timeout
	resultVar <- newEmptyTMVarIO
	result <- withBook $ \bk -> do
		book bk $ forkFlow $ atomically . putTMVar resultVar =<< try io
		atomically $ do
			maybeResult <- tryReadTMVar resultVar
			case maybeResult of
				Just result -> return result
				Nothing -> do
					timedOut <- readTVar delayVar
					if timedOut then throwSTM TimeOutError
					else retry
	case result of
		Right r -> return r
		Left e@(SomeException {}) -> throwIO e

data ReportedError = ReportedError deriving Show
instance Exception ReportedError

data TimeOutError = TimeOutError deriving Show
instance Exception TimeOutError

data Options = Options
	{ optionsUploadFileName :: !String
	, optionsUploadId :: {-# UNPACK #-} !Word64
	, optionsApiKey :: !String
	, optionsDownloadKeyId :: {-# UNPACK #-} !Word64
	, optionsAvCheck :: !Bool
	, optionsOutput :: !String
	, optionsOutputYaml :: !Bool
	, optionsDownloadTimeOut :: {-# UNPACK #-} !Int
	, optionsAVCheckTimeOut :: {-# UNPACK #-} !Int
	, optionsUnpackTimeOut :: {-# UNPACK #-} !Int
	}

main :: IO ()
main = do
	GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

	maybeUploadFileName <- E.lookupEnv "UPLOAD_FILENAME"
	uploadId <- (maybe 0 read) <$> E.lookupEnv "UPLOAD_ID"
	apiKey <- (fromMaybe "") <$> E.lookupEnv "API_KEY"
	downloadKeyId <- (maybe 0 read) <$> E.lookupEnv "DOWNLOAD_KEY_ID"

	let
		parser = O.info (O.helper <*> opts)
			(  O.fullDesc
			<> O.progDesc "Run itchy-runner"
			<> O.header "itchy-runner"
			)
		opts = Options
			<$> O.strOption
				(  O.long "upload-filename"
				<> (case maybeUploadFileName of
					Just uploadFileName -> O.value uploadFileName <> O.showDefault
					Nothing -> mempty
					)
				<> O.metavar "UPLOAD_FILENAME"
				)
			<*> O.option O.auto
				(  O.long "upload-id"
				<> O.value uploadId <> O.showDefault
				<> O.metavar "UPLOAD_ID"
				)
			<*> O.strOption
				(  O.long "api-key"
				<> O.value apiKey <> O.showDefault
				<> O.metavar "API_KEY"
				)
			<*> O.option O.auto
				(  O.long "download-key-id"
				<> O.value downloadKeyId <> O.showDefault
				<> O.metavar "DOWNLOAD_KEY_ID"
				)
			<*> O.switch
				(  O.long "av-check"
				)
			<*> O.strOption
				(  O.long "output"
				<> O.value ""
				<> O.metavar "OUTPUT"
				)
			<*> O.switch
				(  O.long "output-yaml"
				)
			<*> O.option O.auto
				(  O.long "download-timeout"
				<> O.value 60000000 <> O.showDefault
				<> O.metavar "DOWNLOAD_TIMEOUT"
				)
			<*> O.option O.auto
				(  O.long "av-check-timeout"
				<> O.value 60000000 <> O.showDefault
				<> O.metavar "AV_CHECK_TIMEOUT"
				)
			<*> O.option O.auto
				(  O.long "unpack-timeout"
				<> O.value 60000000 <> O.showDefault
				<> O.metavar "UNPACK_TIMEOUT"
				)

	options@Options
		{ optionsOutput = outputFileName
		, optionsOutputYaml = outputYaml
		} <- O.execParser parser
	(run options) `catches`
		[ Handler $ \ReportedError -> return ()
		, Handler $ \(SomeException e) -> report $ \r -> r
			{ report_error = Just $ T.pack $ show e
			}
		]
	(if null outputFileName then B.hPut stdout else B.writeFile outputFileName) .
		(if outputYaml then Y.encode else BL.toStrict . A.encode) =<< readIORef reportRef

run :: Options -> IO ()
run Options
	{ optionsUploadFileName = uploadFileName
	, optionsUploadId = uploadId
	, optionsApiKey = T.pack -> itchToken
	, optionsDownloadKeyId = downloadKeyId
	, optionsAvCheck = avCheck
	, optionsDownloadTimeOut = downloadTimeOut
	, optionsAVCheckTimeOut = avCheckTimeOut
	, optionsUnpackTimeOut = unpackTimeOut
	} = withBook $ \bk -> do

	-- download if needed
	if uploadId > 0 then handleReport (\e r -> r
		{ report_download = ReportDownload_failed e
		}) $ handleTimeout downloadTimeOut $ do
		-- get http manager
		httpManager <- H.getGlobalManager

		-- init itch api
		itchApi <- book bk $ newItchApi httpManager itchToken

		-- get download url
		url <- itchDownloadUpload itchApi (ItchUploadId uploadId) (if downloadKeyId > 0 then Just (ItchDownloadKeyId downloadKeyId) else Nothing)

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
	else report $ \r -> r
		{ report_download = ReportDownload_skipped
		}

	-- AV check
	if avCheck then handleReport (\e r -> r
		{ report_avCheck = ReportAVCheck_failed e
		}) $ handleTimeout avCheckTimeOut $ do
		(exitCode, output, errOutput) <- P.readProcessWithExitCode "clamscan" ["-ria", "--no-summary", uploadFileName] ""
		case exitCode of
			ExitSuccess -> report $ \r -> r
				{ report_avCheck = ReportAVCheck_ok
				}
			ExitFailure _ -> reportError $ \r -> r
				{ report_avCheck = ReportAVCheck_failed $ T.pack output <> T.pack errOutput
				}
	else report $ \r -> r
		{ report_avCheck = ReportAVCheck_skipped
		}

	-- initialize libmagic
	magic <- magicOpen [MagicMimeType]
	magicLoadDefault magic

	-- unpack
	handleReport (\e r -> r
		{ report_unpack = ReportUnpack_failed e
		}) $ handleTimeout unpackTimeOut $ do

		-- parse entries
		let
			parseEntry name path = do
				status <- P.getSymbolicLinkStatus $ T.unpack path
				let P.CMode mode = P.fileMode status
				if P.isRegularFile status then do
					-- get type of file with libmagic
					mime <- T.pack <$> magicFile magic (T.unpack path)
					-- try to parse file
					parses <- parseFile name path mime
					return ReportEntry_file
						{ reportEntry_mode = mode
						, reportEntry_size = fromIntegral $ P.fileSize status
						, reportEntry_mime = mime
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
				M.fromList <$> forM subNames (\(T.pack -> subName) -> (subName, ) <$> parseEntry subName (path <> "/" <> subName))

			parseFile name path mime = do
				parsesRef <- newIORef []
				let addParse parse = modifyIORef' parsesRef (parse :)

				-- ELF (Linux)
				when (mime == "application/x-executable" || mime == "application/x-sharedlib") $ ignoreErrors $ do
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
						, reportDep_version = ReportDepVersion []
						}
					-- calculate glibc version
					let glibcVersions = concat $ flip map (T.lines readelfOutput) $ \case
						(T.words -> (_ : _ : _ : _ : (T.breakOn "@GLIBC_" -> (_, glibcVersion)) : _)) | T.length glibcVersion > 0 -> [T.drop 7 glibcVersion]
						_ -> []
					let deps = if null glibcVersions then neededDeps
						else ReportDep
							{ reportDep_name = "GLIBC"
							, reportDep_version = ReportDepVersion $ maximum $ map (map (read . T.unpack) . T.splitOn ".") glibcVersions
							} : neededDeps
					addParse $ ReportParse_binaryElf ReportBinaryElf
						{ reportBinaryElf_arch = arch
						, reportBinaryElf_isLibrary = mime == "application/x-sharedlib"
						, reportBinaryElf_deps = deps
						}

				-- EXE (Windows)
				when (mime == "application/x-dosexec") $ ignoreErrors $ do
					-- get file format
					objdumpOutput <- T.pack <$> P.readProcess "objdump" ["-a", T.unpack path] ""
					let foldBinary binary line = case reverse $ T.words line of
						(format : "format" : "file" : _) -> case format of
							"pei-x86-64" -> binary
								{ reportBinaryPe_arch = ReportArch_x64
								}
							"pei-i386" -> binary
								{ reportBinaryPe_arch = ReportArch_x86
								}
							_ -> binary
						_ -> binary
					-- check if the binary is CLR (.NET) binary
					isCLR <- do
						peHeader <- withFile (T.unpack path) ReadMode $ \h -> B.hGet h 0x200
						return $ either (const False) id $ flip S.runGet peHeader $ do
							-- according to ECMA-335 spec: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-335.pdf
							-- skip to offset to PE header in MS-DOS header
							S.skip 0x3c
							peFileHeaderOffset <- S.getWord32le
							-- skip to PE header
							S.skip $ fromIntegral peFileHeaderOffset - 0x3c - 4
							-- check PE signature
							peSignature <- S.getBytes 4
							unless (peSignature == "PE\0\0") $ fail "wrong PE signature"
							-- check machine
							machine <- S.getWord16le
							unless (machine == 0x14C) $ fail "wrong machine"
							-- skip to PE optional header
							S.skip $ 20 {- size of PE header -} - 2
							-- skip to CLI header
							S.skip 208
							cliHeaderOffset <- S.getWord32le
							cliHeaderSize <- S.getWord32le
							return $ cliHeaderOffset > 0 && cliHeaderSize > 0

					addParse $ ReportParse_binaryPe $ foldl foldBinary ReportBinaryPe
						{ reportBinaryPe_arch = ReportArch_unknown
						, reportBinaryPe_isLibrary = T.isSuffixOf ".dll" name
						, reportBinaryPe_isCLR = isCLR
						, reportBinaryPe_deps = []
						} $ T.lines objdumpOutput

				-- Mach-O (macOS)
				when (mime == "application/x-mach-binary") $ ignoreErrors $ do
					-- get list of subbinaries and their dependencies
					otoolOutput <- T.pack <$> P.readProcess "otool" ["-hvL", T.unpack path] ""
					let foldSubbinary subbinaries line = case line of
						(T.words -> ("MH_MAGIC_64" : "X86_64" : _)) -> ReportMachOSubBinary
							{ reportMachoSubBinary_arch = ReportArch_x64
							, reportMachoSubBinary_deps = []
							} : subbinaries
						(T.words -> ("MH_MAGIC" : "I386" : _)) -> ReportMachOSubBinary
							{ reportMachoSubBinary_arch = ReportArch_x86
							, reportMachoSubBinary_deps = []
							} : subbinaries
						(reverse . T.words -> (_depCurrentVersion : "version" : "current" : depCompatibilityVersion : "version" : "(compatibility" : depName)) -> case subbinaries of
							subbinary : restSubbinaries -> subbinary
								{ reportMachoSubBinary_deps = ReportDep
									{ reportDep_name = T.intercalate " " depName -- may be wrong :(
									, reportDep_version = ReportDepVersion $ map (read . T.unpack) $ T.splitOn "." $ fromMaybe depCompatibilityVersion $ T.stripSuffix "," depCompatibilityVersion
									} : reportMachoSubBinary_deps subbinary
								} : restSubbinaries
							[] -> []
						_ -> subbinaries
					addParse $ ReportParse_binaryMachO ReportBinaryMachO
						{ reportBinaryMachO_binaries = foldl foldSubbinary [] $ T.lines otoolOutput
						, reportBinaryMachO_isLibrary = T.isSuffixOf ".dylib" name
						}

				-- parse .itch.toml
				when (name == ".itch.toml") $ addParse =<< let
					f = ReportParse_itchToml . \case
						Right (Right tomlDoc) -> case A.fromJSON $ A.toJSON tomlDoc of
							A.Success itchToml -> Right itchToml
							A.Error itchTomlErr -> Left $ T.pack itchTomlErr
						Right (Left tomlErr) -> Left $ T.pack $ show tomlErr
						Left (SomeException e) -> Left $ T.pack $ show e
					in f <$> try (Toml.parseTomlDoc ".itch.toml" <$> T.readFile (T.unpack path))

				-- parse archives with unar
				when
					(  mime == "application/x-tar"
					|| mime == "application/x-gzip"
					|| mime == "application/x-bzip2"
					|| mime == "application/x-xz"
					|| mime == "application/x-rar"
					|| mime == "application/vnd.debian.binary-package"
					) $ ignoreErrors $ do
					entries <- withTempDirectory (takeDirectory $ T.unpack path) (T.unpack name) $ \archiveUnpackPath -> do
						P.callProcess "unar" ["-q", "-s", "-D", "-o", archiveUnpackPath, T.unpack path]
						parseSubEntries $ T.pack archiveUnpackPath
					addParse $ ReportParse_archive ReportArchive
						{ reportArchive_entries = entries
						}

				-- parse archives with 7z
				when
					(  mime == "application/zip"
					|| mime == "application/x-7z-compressed"
					|| T.isSuffixOf ".dmg" name
					) $ ignoreErrors $ do
					entries <- withTempDirectory (takeDirectory $ T.unpack path) (T.unpack name) $ \archiveUnpackPath -> do
						void $ P.readProcess "7z" ["x", "-o" ++ archiveUnpackPath, T.unpack path] ""
						parseSubEntries $ T.pack archiveUnpackPath
					addParse $ ReportParse_archive ReportArchive
						{ reportArchive_entries = entries
						}

				-- parse .msi
				when (mime == "application/x-msi") $ ignoreErrors $ do
					entries <- withTempDirectory (takeDirectory $ T.unpack path) (T.unpack name) $ \msiUnpackPath -> do
						void $ P.readProcess "msiextract" ["-C", msiUnpackPath, T.unpack path] ""
						parseSubEntries $ T.pack msiUnpackPath
					addParse $ ReportParse_msi ReportMsi
						{ reportMsi_entries = entries
						}

				readIORef parsesRef

		rootEntry <- parseEntry (T.pack uploadFileName) (T.pack uploadFileName)

		report $ \r -> r
			{ report_unpack = ReportUnpack_succeeded $ M.singleton (T.pack uploadFileName) rootEntry
			}
