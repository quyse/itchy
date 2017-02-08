{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Y
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified System.Process as P

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
	putStrLn . T.unpack . T.decodeUtf8 . Y.encode =<< readIORef reportRef

run :: IO ()
run = withBook $ \bk -> do
	uploadFileName <- getEnv "ITCHIO_UPLOAD_FILENAME"

	-- download if needed
	maybeUploadId <- (read <$>) <$> lookupEnv "ITCHIO_UPLOAD_ID"
	case maybeUploadId of
		Just uploadId -> handleReport (\e r -> r
			{ report_download = ReportDownload_failed e
			}) $ do
			-- get http manager
			httpManager <- H.getGlobalManager

			-- init itch api
			itchToken <- T.pack <$> getEnv "ITCHIO_API_KEY"
			itchApi <- book bk $ newItchApi httpManager itchToken

			-- try to get download key id
			maybeDownloadKeyId <- (read <$>) <$> lookupEnv "ITCHIO_DOWNLOAD_KEY_ID"

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
		report $ \r -> r
			{ report_unpack = ReportUnpack_succeeded
			}
