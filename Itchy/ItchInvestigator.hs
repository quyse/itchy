{-|
Module: Itchy.ItchInvestigator
Description: Makes reports about itch uploads.
-}

module Itchy.ItchInvestigator
	( ItchInvestigator()
	, newItchInvestigator
	, ItchInvestigation(..)
	, investigateItchUpload
	) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Aeson as A
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.Types
import System.Posix.Time
import qualified System.Process as P

import Flaw.Book
import Flaw.Flow

import Itchy.Itch
import Itchy.ItchCache
import Itchy.Report

data ItchInvestigator = ItchInvestigator
	{ itchInvestigatorItchCache :: !ItchCache
	, itchInvestigatorFlow :: !Flow
	, itchInvestigatorStalePeriod :: {-# UNPACK #-} !Int64
	, itchInvestigatorRefreshingVar :: {-# UNPACK #-} !(TVar (M.Map ItchUploadId (Either Int ())))
	, itchInvestigatorProcessingQueue :: {-# UNPACK #-} !(TQueue (ItchUploadId, T.Text))
	, itchInvestigatorNextQueuedVar :: {-# UNPACK #-} !(TVar Int)
	, itchInvestigatorFirstQueuedVar :: {-# UNPACK #-} !(TVar Int)
	}

newItchInvestigator :: ItchCache -> T.Text -> Int -> Int64 -> IO (ItchInvestigator, IO ())
newItchInvestigator itchCache apiKey threadsCount stalePeriod = withSpecialBook $ \bk -> do
	flow <- book bk newFlow
	refreshingVar <- newTVarIO M.empty
	processingQueue <- newTQueueIO
	nextQueuedVar <- newTVarIO 0
	firstQueuedVar <- newTVarIO 0

	-- spawn threads
	let
		thread = forever $ do
			(uploadId@(ItchUploadId uploadIdInt), uploadFileName) <- atomically $ do
				pair@(uploadId, _) <- readTQueue processingQueue
				modifyTVar' refreshingVar $ M.insert uploadId $ Right ()
				modifyTVar' firstQueuedVar (+ 1)
				return pair
			print ("start investigation", uploadId)
			-- call docker
			maybeReport <- A.decodeStrict' . T.encodeUtf8 . T.pack <$> P.readProcess "docker"
				[ "run"
				, "--rm"
				, "itchy-runner" -- image
				, "itchy-runner" -- command
				, "--upload-filename", T.unpack uploadFileName
				, "--upload-id", show uploadIdInt
				, "--api-key", T.unpack apiKey
				, "--av-check"
				] ""
			-- put in cache
			atomically $ asyncRunInFlow flow $ do
				CTime currentTime <- epochTime
				itchCachePutReport itchCache uploadId CachedReport
					{ cachedReport_maybeReport = maybeReport
					, cachedReport_updated = currentTime
					}
				atomically $ modifyTVar' refreshingVar $ M.delete uploadId
			print ("finish investigation", uploadId)

		f i = when (i < threadsCount) $ do
			book bk $ forkFlow thread
			f $ i + 1
		in f 0

	return ItchInvestigator
		{ itchInvestigatorItchCache = itchCache
		, itchInvestigatorFlow = flow
		, itchInvestigatorStalePeriod = stalePeriod
		, itchInvestigatorRefreshingVar = refreshingVar
		, itchInvestigatorProcessingQueue = processingQueue
		, itchInvestigatorNextQueuedVar = nextQueuedVar
		, itchInvestigatorFirstQueuedVar = firstQueuedVar
		}

data ItchInvestigation
	= ItchStartedInvestigation
	| ItchQueuedInvestigation {-# UNPACK #-} !Int
	| ItchProcessingInvestigation
	| ItchInvestigation
		{ itchInvestigationMaybeReport :: !(Maybe Report)
		, itchInvestigationTime :: {-# UNPACK #-} !Int64
		}

investigateItchUpload :: ItchInvestigator -> ItchUploadId -> T.Text -> Bool -> IO ItchInvestigation
investigateItchUpload ItchInvestigator
	{ itchInvestigatorItchCache = itchCache
	, itchInvestigatorFlow = flow
	, itchInvestigatorStalePeriod = stalePeriod
	, itchInvestigatorRefreshingVar = refreshingVar
	, itchInvestigatorProcessingQueue = processingQueue
	, itchInvestigatorNextQueuedVar = nextQueuedVar
	, itchInvestigatorFirstQueuedVar = firstQueuedVar
	} uploadId uploadFileName autoRefresh = runInFlow flow $ do
	maybeCachedReport <- itchCacheGetReport itchCache uploadId
	case maybeCachedReport of
		Just CachedReport
			{ cachedReport_maybeReport = maybeReport
			, cachedReport_updated = updated
			} -> do
			CTime currentTime <- epochTime
			if autoRefresh && updated < currentTime - stalePeriod then refresh
			else return ItchInvestigation
				{ itchInvestigationMaybeReport = maybeReport
				, itchInvestigationTime = updated
				}
		Nothing -> refresh
	where refresh = atomically $ do
		refreshing <- readTVar refreshingVar
		case M.lookup uploadId refreshing of
			Just entry -> case entry of
				Left number -> do
					firstQueued <- readTVar firstQueuedVar
					return $ ItchQueuedInvestigation $ number - firstQueued
				Right () -> return ItchProcessingInvestigation
			Nothing -> do
				nextQueued <- readTVar nextQueuedVar
				writeTVar nextQueuedVar $! nextQueued + 1
				writeTVar refreshingVar $ M.insert uploadId (Left nextQueued) refreshing
				writeTQueue processingQueue (uploadId, uploadFileName)
				return ItchStartedInvestigation
