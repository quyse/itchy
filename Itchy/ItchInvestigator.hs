{-|
Module: Itchy.ItchInvestigator
Description: Makes reports about itch uploads.
-}

module Itchy.ItchInvestigator
	( ItchInvestigator()
	, newItchInvestigator
	, investigateItchUpload
	) where

import qualified Data.Aeson as A
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
	-- call docker
	maybeReport <- A.decodeStrict' . T.encodeUtf8 . T.pack <$> P.readProcess "docker"
		[ "run"
		, "--rm"
		, "itchy-runner" -- image
		, "itchy-runner" -- command
		, "--upload-filename", T.unpack uploadFileName
		, "--upload-id", show uploadId
		, "--api-key", T.unpack apiKey
		, "--av-check"
		] ""
	-- call callback
	maybe (return ()) callback maybeReport
