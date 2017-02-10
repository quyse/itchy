{-# LANGUAGE OverloadedStrings, PatternSynonyms, TupleSections, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Main(main) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y
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
import qualified Text.Toml.Types as Toml

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
	, report_itchToml = ReportItchToml_notStarted
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

	-- unpack
	handleReport (\e r -> r
		{ report_unpack = ReportUnpack_failed e
		}) $ do
		P.callProcess "unar" ["-q", "-o", WORK_DIR, uploadFileName]

		-- parse entries
		let
			parseEntry path = do
				status <- P.getFileStatus $ T.unpack path
				let P.CMode mode = P.fileMode status
				if P.isRegularFile status then return ReportEntry_file
					{ reportEntry_mode = mode
					, reportEntry_size = fromIntegral $ P.fileSize status
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
				HM.fromList <$> forM subNames (\(T.pack -> subName) -> (subName, ) <$> parseEntry (path <> "/" <> subName))

		entries <- parseSubEntries WORK_DIR

		report $ \r -> r
			{ report_unpack = ReportUnpack_succeeded entries
			}

	-- parse .itch.toml
	handleIsolatedReport (\e r -> r
		{ report_itchToml = ReportItchToml_wrong e
		}) $ do
		let reportItchTomlError e = reportError $ \r -> r
			{ report_itchToml = ReportItchToml_wrong e
			}

		-- choose .itch.toml
		unpack <- report_unpack <$> readIORef reportRef
		itchTomlPath <- case unpack of
			-- .itch.toml at the root
			ReportUnpack_succeeded
				( HM.lookup ".itch.toml" ->
					Just (ReportEntry_file {})
				) -> return "/.itch.toml"
			-- .itch.toml in a single directory
			ReportUnpack_succeeded
				( HM.toList ->
					[ ( directoryName
						, ReportEntry_directory
							{ reportEntry_entries = HM.lookup ".itch.toml" -> Just (ReportEntry_file {})
							}
						)
					]
				) -> return $ directoryName <> "/.itch.toml"
			-- whatever else is unsupported
			_ -> reportError $ \r -> r
				{ report_itchToml = ReportItchToml_missing
				}

		eitherTomlDoc <- Toml.parseTomlDoc ".itch.toml" <$> T.readFile (T.unpack $ WORK_DIR <> "/" <> itchTomlPath)
		itchToml <- case eitherTomlDoc of
			Right rootTable -> do
				prereqs <- case HM.lookup "prereqs" rootTable of
					Just prereqsNode -> case prereqsNode of
						Toml.VTArray prereqsArray -> forM prereqsArray $ \prereqTable -> do
							name <- case HM.lookup "name" prereqTable of
								Just (Toml.VString name) -> return name
								Just _ -> reportItchTomlError "prereq name must be a string"
								Nothing -> reportItchTomlError "prereq must have a name"
							return ReportItchTomlPrereq
								{ reportItchTomlPrereq_name = name
								}
						_ -> reportItchTomlError "prereqs must be an array of tables"
					Nothing -> return V.empty
				actions <- case HM.lookup "actions" rootTable of
					Just actionsNode -> case actionsNode of
						Toml.VTArray actionsArray -> forM actionsArray $ \actionTable -> do
							actionName <- case HM.lookup "name" actionTable of
								Just (Toml.VString name) -> return name
								Just _ -> reportItchTomlError "action name must be a string"
								Nothing -> reportItchTomlError "action must have a name"
							actionPath <- case HM.lookup "path" actionTable of
								Just (Toml.VString path) -> return path
								Just _ -> reportItchTomlError "action path must be a string"
								Nothing -> reportItchTomlError "action must have a path"
							actionIcon <- case HM.lookup "icon" actionTable of
								Just (Toml.VString icon) -> return icon
								Just _ -> reportItchTomlError "action icon must be a string"
								Nothing -> return ""
							actionScope <- case HM.lookup "scope" actionTable of
								Just (Toml.VString scope) -> return scope
								Just _ -> reportItchTomlError "action scope must be a string"
								Nothing -> return ""
							actionArgs <- case HM.lookup "args" actionTable of
								Just (Toml.VArray argsArray) -> forM argsArray $ \argNode -> case argNode of
									Toml.VString arg -> return arg
									_ -> reportItchTomlError "action arg must be a string"
								Just _ -> reportItchTomlError "action args must be an array of strings"
								Nothing -> return V.empty
							-- action without locales
							let action = ReportItchTomlAction
								{ reportItchTomlAction_name = actionName
								, reportItchTomlAction_path = actionPath
								, reportItchTomlAction_icon = actionIcon
								, reportItchTomlAction_scope = actionScope
								, reportItchTomlAction_args = V.toList actionArgs
								, reportItchTomlAction_locales = HM.empty
								}
							locales <- case HM.lookup "locales" actionTable of
								Just (Toml.VTable localesTable) -> forM localesTable $ \localeNode -> case localeNode of
									Toml.VTable localeTable -> do
										localizedActionName <- case HM.lookup "name" localeTable of
											Just (Toml.VString name) -> return name
											Just _ -> reportItchTomlError "localized action name must be a string"
											Nothing -> return actionName
										return action
											{ reportItchTomlAction_name = localizedActionName
											}
									_ -> reportItchTomlError "action locale must be a table"
								Just _ -> reportItchTomlError "action locales must be a table"
								Nothing -> return HM.empty
							return action
								{ reportItchTomlAction_locales = locales
								}
						_ -> reportItchTomlError "actions must be an array of tables"
					Nothing -> return V.empty
				return ReportItchToml_ok
					{ reportItchToml_prereqs = V.toList prereqs
					, reportItchToml_actions = V.toList actions
					}
			Left _e -> reportError $ \r -> r
				{ report_itchToml = ReportItchToml_malformed
				}
		report $ \r -> r
			{ report_itchToml = itchToml
			}

pattern WORK_DIR = "package"
