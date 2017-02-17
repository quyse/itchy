{-|
Module: Itchy.ItchCache
Description: Cache for itch data
-}

{-# LANGUAGE DeriveGeneric, LambdaCase #-}

module Itchy.ItchCache
	( ItchCache()
	, newItchCache
	, itchCacheGetGame
	, itchCacheGetUpload
	, itchCacheGetReport
	, itchCachePutReport
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Int
import qualified Data.Set as S
import qualified Data.Serialize as S
import qualified Data.Text as T
import Foreign.C.Types
import GHC.Generics(Generic)
import System.Posix.Time

import Flaw.Book
import Flaw.Data.Lmdb
import Flaw.Flow

import Itchy.Itch
import Itchy.Report

data ItchCache = ItchCache
	{ itchCacheItchApi :: !ItchApi
	, itchCacheDb :: !Lmdb
	, itchCacheFlow :: !Flow
	-- | In-progress refreshes.
	, itchCacheRefreshingGamesVar :: {-# UNPACK #-} !(TVar (S.Set ItchGameId))
	-- | Stale period in seconds.
	, itchCacheStalePeriod :: {-# UNPACK #-} !Int64
	-- | Delay between API requests in microseconds.
	, itchCacheApiCooldown :: {-# UNPACK #-} !Int
	}

-- | Database key.
data CacheKey
	= CacheKeyGame {-# UNPACK #-} !ItchGameId
	| CacheKeyUpload {-# UNPACK #-} !ItchUploadId
	| CacheKeyReport {-# UNPACK #-} !ItchUploadId

instance S.Serialize CacheKey where
	put = \case
		CacheKeyGame (ItchGameId gameId) -> do
			S.putWord8 0
			S.putWord64le gameId
		CacheKeyUpload (ItchUploadId uploadId) -> do
			S.putWord8 1
			S.putWord64le uploadId
		CacheKeyReport (ItchUploadId uploadId) -> do
			S.putWord8 2
			S.putWord64le uploadId
	get = do
		t <- S.getWord8
		case t of
			0 -> CacheKeyGame . ItchGameId <$> S.getWord64le
			1 -> CacheKeyUpload . ItchUploadId <$> S.getWord64le
			2 -> CacheKeyReport . ItchUploadId <$> S.getWord64le
			_ -> fail "wrong key type"

data CachedGame = CachedGame
	{ game_maybeItchGameWithUploads :: !(Maybe (ItchGame, [ItchUploadId]))
	, game_updated :: {-# UNPACK #-} !Int64
	} deriving Generic
instance S.Serialize CachedGame

data CachedUpload = CachedUpload
	{ upload_maybeItchUpload :: !(Maybe ItchUpload)
	, upload_updated :: {-# UNPACK #-} !Int64
	} deriving Generic
instance S.Serialize CachedUpload

newItchCache :: ItchApi -> T.Text -> Int64 -> Int -> IO (ItchCache, IO ())
newItchCache itchApi dbFileName stalePeriod apiCooldown = withSpecialBook $ \bk -> do
	db <- book bk $ lmdbOpen dbFileName (1024 * 1024 * 1024)
	flow <- book bk newFlow
	refreshingGamesVar <- newTVarIO S.empty
	return ItchCache
		{ itchCacheItchApi = itchApi
		, itchCacheDb = db
		, itchCacheFlow = flow
		, itchCacheRefreshingGamesVar = refreshingGamesVar
		, itchCacheStalePeriod = stalePeriod
		, itchCacheApiCooldown = apiCooldown
		}

itchCacheGet :: S.Serialize tvalue => (tid -> CacheKey) -> ItchCache -> tid -> IO (Maybe tvalue)
itchCacheGet idToKey ItchCache
	{ itchCacheDb = db
	} keyId = do
	maybeValue <- lmdbRead db $ \txn -> lmdbGet txn $ S.encode $ idToKey keyId
	return $ case maybeValue of
		Just value -> either (const Nothing) Just $ S.decode value
		Nothing -> Nothing

itchCachePut :: S.Serialize tvalue => (tid -> CacheKey) -> ItchCache -> tid -> tvalue -> IO ()
itchCachePut idToKey ItchCache
	{ itchCacheDb = db
	} keyId value = lmdbWrite db $ \txn -> do
	lmdbPut txn (S.encode $ idToKey keyId) (S.encode value)
	lmdbCommit txn

-- | Get if given unix time is stale.
itchCacheIsStale :: ItchCache -> Int64 -> IO Bool
itchCacheIsStale ItchCache
	{ itchCacheStalePeriod = stalePeriod
	} updated = do
	CTime currentTime <- epochTime
	return $ updated + stalePeriod < currentTime

itchCacheOperation :: ItchCache -> IO () -> IO ()
itchCacheOperation ItchCache
	{ itchCacheFlow = flow
	, itchCacheApiCooldown = apiCooldown
	} io = atomically $ asyncRunInFlow flow $ do
	io
	threadDelay apiCooldown

itchCacheGetGame :: ItchCache -> ItchGameId -> IO (Maybe (ItchGame, [ItchUpload]))
itchCacheGetGame cache gameId = do
	-- try to get game from database
	maybeGame <- itchCacheGet CacheKeyGame cache gameId

	needRefresh <- case maybeGame of
		Just CachedGame
			{ game_updated = updated
			} -> itchCacheIsStale cache updated
		Nothing -> return True

	-- if data is stale or not exist, queue refreshing
	when needRefresh $ itchCacheRefreshGame cache gameId

	-- get uploads and return result
	case maybeGame of
		Just CachedGame
			{ game_maybeItchGameWithUploads = Just (itchGame, uploadIds)
			} -> do
			itchUploads <- (concat <$>) . forM uploadIds $ \uploadId -> do
				-- try to get uploads from database
				maybeUpload <- itchCacheGet CacheKeyUpload cache uploadId
				case maybeUpload of
					Just CachedUpload
						{ upload_maybeItchUpload = Just upload
						} -> return [upload]
					_ -> return []

			return $ Just (itchGame, itchUploads)
		_ -> return Nothing

itchCacheRefreshGame :: ItchCache -> ItchGameId -> IO ()
itchCacheRefreshGame cache@ItchCache
	{ itchCacheItchApi = itchApi
	, itchCacheRefreshingGamesVar = refreshingGamesVar
	, itchCacheApiCooldown = apiCooldown
	} gameId = join $ atomically $ do
	isRefreshing <- S.member gameId <$> readTVar refreshingGamesVar
	if isRefreshing then return $ return ()
	else do
		modifyTVar' refreshingGamesVar $ S.insert gameId
		return $ itchCacheOperation cache $ flip finally (atomically $ modifyTVar' refreshingGamesVar $ S.delete gameId) $ do
			eitherUpdatedItchGame <- itchGetGame itchApi gameId
			CTime currentTime <- epochTime
			updatedGame <- case eitherUpdatedItchGame of
				Right updatedItchGame -> do
					-- delay
					threadDelay apiCooldown
					-- get uploads
					updatedUploads <- (either (const []) id) <$> itchGetGameUploads itchApi gameId Nothing
					-- save uploads in cache
					forM_ updatedUploads $ \upload@ItchUpload
						{ itchUpload_id = uploadId
						} -> itchCachePut CacheKeyUpload cache uploadId CachedUpload
						{ upload_maybeItchUpload = Just upload
						, upload_updated = currentTime
						}
					return CachedGame
						{ game_maybeItchGameWithUploads = Just (updatedItchGame, map itchUpload_id updatedUploads)
						, game_updated = currentTime
						}
				Left _ -> return CachedGame
					{ game_maybeItchGameWithUploads = Nothing
					, game_updated = currentTime
					}
			-- save game in cache
			itchCachePut CacheKeyGame cache gameId updatedGame

itchCacheGetUpload :: ItchCache -> ItchUploadId -> IO (Maybe ItchUpload)
itchCacheGetUpload cache uploadId = do
	maybeCachedUpload <- itchCacheGet CacheKeyUpload cache uploadId
	return $ maybeCachedUpload >>= upload_maybeItchUpload

itchCacheGetReport :: ItchCache -> ItchUploadId -> IO (Maybe Report)
itchCacheGetReport = itchCacheGet CacheKeyReport

itchCachePutReport :: ItchCache -> ItchUploadId -> Report -> IO ()
itchCachePutReport = itchCachePut CacheKeyReport
