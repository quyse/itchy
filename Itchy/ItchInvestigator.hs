{-|
Module: Itchy.ItchInvestigator
Description: Makes reports about itch uploads.
-}

module Itchy.ItchInvestigator
	( ItchInvestigator()
	, newItchInvestigator
	, investigateItchUpload
	) where

import qualified Data.ByteString as B
import Control.Concurrent.STM
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified System.Posix.Files as P
import qualified System.Posix.Temp as P
import qualified System.Process as P

import Flaw.Book
import Flaw.Flow

import Itchy.Itch
import Itchy.Report

data ItchInvestigator = ItchInvestigator
	{ itchInvestigatorApiKey :: !T.Text
	, itchInvestigatorFlow :: !Flow
	}

newItchInvestigator :: T.Text -> IO (ItchInvestigator, IO ())
newItchInvestigator apiKey = withSpecialBook $ \bk -> do
	flow <- book bk newFlow
	return ItchInvestigator
		{ itchInvestigatorApiKey = apiKey
		, itchInvestigatorFlow = flow
		}

investigateItchUpload :: ItchInvestigator -> ItchUploadId -> T.Text -> (Report -> IO ()) -> IO ()
investigateItchUpload ItchInvestigator
	{ itchInvestigatorApiKey = apiKey
	, itchInvestigatorFlow = flow
	} (ItchUploadId uploadId) uploadFileName callback = atomically $ asyncRunInFlow flow $ do
	-- create temporary directory
	workPath <- P.mkdtemp "/tmp/itchyrun"
	P.setFileMode workPath 0o777
	-- call docker
	P.callProcess "docker"
		[ "run"
		, "--rm"
		, "-v", workPath ++ ":/data"
		, "itchy-runner" -- image
		, "itchy-runner" -- command
		, "--upload-filename", T.unpack uploadFileName
		, "--upload-id", show uploadId
		, "--api-key", T.unpack apiKey
		, "--av-check"
		, "--output", "/data/report.bin"
		]
	-- read report and call callback
	reportBytes <- B.readFile $ workPath ++ "/report.bin"
	case S.decode reportBytes of
		Right report -> callback report
		Left _ -> return ()
