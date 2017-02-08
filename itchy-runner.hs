{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main(main) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import qualified System.Directory as D
import qualified System.Environment as E
import System.IO
import System.IO.Unsafe
import qualified System.Process as P
import qualified System.Posix as P

import Flaw.Book

import Itchy.Itch
import Itchy.Report

{-# NOINLINE reportRef #-}
reportRef :: IORef Report
reportRef = unsafePerformIO $ newIORef Report
	{ report_error = Nothing
	, report_download = ReportDownload_notStarted
	, report_unpack = ReportUnpack_notStarted
	}

report :: (Report -> Report) -> IO ()
report = modifyIORef' reportRef

reportError :: (Report -> Report) -> IO a
reportError f = do
	report f
	throwIO ReportedError

handleReport :: (T.Text -> Report -> Report) -> IO a -> IO a
handleReport h io = catch io $ \(SomeException e) -> reportError $ h $ T.pack $ show e

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

	-- unpack
	handleReport (\e r -> r
		{ report_unpack = ReportUnpack_failed e
		}) $ do
		P.callProcess "unar" ["-q", "-o", "work", uploadFileName]

		-- parse entries
		let
			parseEntry name path = do
				status <- P.getFileStatus $ T.unpack path
				let P.CMode mode = P.fileMode status
				if P.isRegularFile status then return ReportEntry_file
					{ reportEntry_name = name
					, reportEntry_mode = mode
					, reportEntry_size = fromIntegral $ P.fileSize status
					}
				else if P.isDirectory status then do
					subEntries <- parseSubEntries path
					return ReportEntry_directory
						{ reportEntry_name = name
						, reportEntry_mode = mode
						, reportEntry_entries = subEntries
						}
				else if P.isSymbolicLink status then do
					link <- P.readSymbolicLink $ T.unpack path
					return ReportEntry_symlink
						{ reportEntry_name = name
						, reportEntry_mode = mode
						, reportEntry_link = T.pack link
						}
				else return ReportEntry_unknown
					{ reportEntry_name = name
					, reportEntry_mode = mode
					}
			parseSubEntries path = do
				subNames <- D.listDirectory $ T.unpack path
				forM subNames $ \(T.pack -> subName) -> parseEntry subName $ path <> "/" <> subName

		entries <- parseSubEntries "work"

		report $ \r -> r
			{ report_unpack = ReportUnpack_succeeded entries
			}
