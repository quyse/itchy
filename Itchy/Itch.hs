{-|
Module: Itchy.Itch
Description: Itch API
-}

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings, ViewPatterns #-}

module Itchy.Itch
	( ItchApi()
	, newItchApi
	, itchJwtMe
	, itchGetUser
	, itchGetGame
	, itchGetGameUploads
	, itchDownloadUpload
	, itchGetUploadBuilds
	, itchGetBuild
	, itchSearchGame
	, ItchUserId(..)
	, ItchUser(..)
	, ItchGameId(..)
	, ItchGame(..)
	, ItchGameShort(..)
	, ItchUploadId(..)
	, ItchUpload(..)
	, ItchBuildId(..)
	, ItchBuild(..)
	, ItchDownloadKeyId(..)
	) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.Monoid
import qualified Data.Serialize as S
import Data.Serialize.Text()
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Word
import GHC.Generics(Generic)
import qualified Network.HTTP.Client as H

data ItchApi = ItchApi
	{ itchHttpManager :: !H.Manager
	, itchHttpRequest :: !H.Request
	}

newItchApi :: H.Manager -> T.Text -> IO (ItchApi, IO ())
newItchApi httpManager token = return (ItchApi
	{ itchHttpManager = httpManager
	, itchHttpRequest = H.defaultRequest
		{ H.method = "GET"
		, H.secure = True
		, H.host = "itch.io"
		, H.port = 443
		, H.path = "/api/1/" <> T.encodeUtf8 token
		}
	}, return ())

itchRequest :: (A.FromJSON a, Show a) => ItchApi -> B.ByteString -> [(B.ByteString, Maybe B.ByteString)] -> IO a
itchRequest ItchApi
	{ itchHttpManager = httpManager
	, itchHttpRequest = httpRequest
	} path params = do
	response <- H.httpLbs (H.setQueryString params httpRequest
		{ H.path = H.path httpRequest <> path
		}) httpManager
	return $ case A.eitherDecode' (H.responseBody response) of
		Right v -> v
		Left e -> error e

itchJwtMe :: ItchApi -> T.Text -> IO ItchUser
itchJwtMe ItchApi
	{ itchHttpManager = httpManager
	, itchHttpRequest = httpRequest
	} token = do
	response <- H.httpLbs httpRequest
		{ H.path = "/api/1/jwt/me"
		, H.requestHeaders = ("Authorization", T.encodeUtf8 token) : H.requestHeaders httpRequest
		} httpManager
	return $ case A.eitherDecode' (H.responseBody response) of
		Right ItchJwtResponse
			{ itchJwtResponse_user = user
			} -> user
		Left e -> error e

itchGetUser :: ItchApi -> ItchUserId -> IO ItchUser
itchGetUser api (ItchUserId userId) = itchRequest api ("/users/" <> (fromString $ show userId)) []

itchGetGame :: ItchApi -> ItchGameId -> IO (Either (V.Vector T.Text) ItchGame)
itchGetGame api (ItchGameId gameId) = do
	response <- itchRequest api ("/game/" <> fromString (show gameId)) []
	case response of
		ItchGameResponse
			{ itchGameResponse_game = Just game
			} -> return $ Right game
		ItchGameResponse
			{ itchGameResponse_errors = Just errors
			} -> return $ Left errors
		_ -> return $ Left V.empty

itchGetGameUploads :: ItchApi -> ItchGameId -> Maybe ItchDownloadKeyId -> IO (Either (V.Vector T.Text) (V.Vector ItchUpload))
itchGetGameUploads api (ItchGameId gameId) maybeDownloadKeyId = do
	let path = case maybeDownloadKeyId of
		Just (ItchDownloadKeyId downloadKeyId) -> "/download-key/" <> fromString (show downloadKeyId) <> "/uploads"
		Nothing -> "/game/" <> fromString (show gameId) <> "/uploads"
	itchParseArrayResponse "uploads" <$> itchRequest api path []

itchDownloadUpload :: ItchApi -> ItchUploadId -> Maybe ItchDownloadKeyId -> IO T.Text
itchDownloadUpload api (ItchUploadId uploadId) maybeDownloadKeyId = itchUrlResponse_url <$> itchRequest api ("/upload/" <> T.encodeUtf8 (T.pack $ show uploadId) <> "/download") params where
	params = case maybeDownloadKeyId of
		Just (ItchDownloadKeyId downloadKeyId) -> [("download_key_id", Just $ T.encodeUtf8 $ T.pack $ show downloadKeyId)]
		Nothing -> []

itchGetUploadBuilds :: ItchApi -> ItchUploadId -> Maybe ItchDownloadKeyId -> IO (Either (V.Vector T.Text) (V.Vector ItchBuild))
itchGetUploadBuilds api (ItchUploadId uploadId) maybeDownloadKeyId = do
	responseValue <- itchRequest api ("/upload/" <> T.encodeUtf8 (T.pack $ show uploadId) <> "/builds") $ case maybeDownloadKeyId of
		Just (ItchDownloadKeyId downloadKeyId) -> [("download_key_id", Just $ T.encodeUtf8 $ T.pack $ show downloadKeyId)]
		Nothing -> []
	return $ itchParseArrayResponse "builds" responseValue

itchGetBuild :: ItchApi -> ItchUploadId -> ItchBuildId -> Maybe ItchDownloadKeyId -> IO (Either (V.Vector T.Text) ItchBuild)
itchGetBuild api (ItchUploadId uploadId) (ItchBuildId buildId) maybeDownloadKeyId = do
	response <- itchRequest api ("/upload/" <> T.encodeUtf8 (T.pack $ show uploadId) <> "/builds/" <> T.encodeUtf8 (T.pack $ show buildId)) $ case maybeDownloadKeyId of
		Just (ItchDownloadKeyId downloadKeyId) -> [("download_key_id", Just $ T.encodeUtf8 $ T.pack $ show downloadKeyId)]
		Nothing -> []
	case response of
		ItchBuildResponse
			{ itchBuildResponse_build = Just build
			} -> return $ Right build
		ItchBuildResponse
			{ itchBuildResponse_errors = Just errors
			} -> return $ Left errors
		_ -> return $ Left V.empty

itchSearchGame :: ItchApi -> T.Text -> IO (V.Vector ItchGameShort)
itchSearchGame api query = itchGamesShortResponse_games <$> itchRequest api "/search/games" [("query", Just (T.encodeUtf8 query))]

itchParseArrayResponse :: A.FromJSON a => T.Text -> A.Value -> Either (V.Vector T.Text) (V.Vector a)
itchParseArrayResponse fieldName = \case
	A.Object (HM.lookup fieldName -> Just thingsValue) -> case thingsValue of
		A.Array {} -> case A.parseEither A.parseJSON thingsValue of
			Right things -> Right things
			Left e -> Left $ V.singleton $ T.pack e
		-- handle empty object instead of array when no things
		A.Object (HM.null -> True) -> Right V.empty
		_ -> Left $ V.singleton "wrong things array"
	A.Object (HM.lookup "errors" -> Just errorsValue) -> case A.parseEither A.parseJSON errorsValue of
		Right errors -> Left errors
		Left e -> Left $ V.singleton $ T.pack e
	_ -> Left $ V.singleton "wrong json"

newtype ItchUserId = ItchUserId Word64 deriving (Generic, Show, S.Serialize, A.FromJSON)
data ItchUser = ItchUser
	{ itchUser_id :: !ItchUserId
	, itchUser_username :: !T.Text
	, itchUser_url :: !T.Text
	, itchUser_cover_url :: !(Maybe T.Text)
	} deriving (Generic, Show)
instance S.Serialize ItchUser
instance A.FromJSON ItchUser where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 9
		}

data ItchJwtResponse = ItchJwtResponse
	{ itchJwtResponse_user :: !ItchUser
	} deriving (Generic, Show)
instance A.FromJSON ItchJwtResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 16
		}

data ItchGameResponse = ItchGameResponse
	{ itchGameResponse_game :: !(Maybe ItchGame)
	, itchGameResponse_errors :: !(Maybe (V.Vector T.Text))
	} deriving (Generic, Show)
instance A.FromJSON ItchGameResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 17
		}

data ItchGamesShortResponse = ItchGamesShortResponse
	{ itchGamesShortResponse_games :: !(V.Vector ItchGameShort)
	} deriving (Generic, Show)
instance A.FromJSON ItchGamesShortResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 23
		}

data ItchBuildResponse = ItchBuildResponse
	{ itchBuildResponse_build :: !(Maybe ItchBuild)
	, itchBuildResponse_errors :: !(Maybe (V.Vector T.Text))
	} deriving (Generic, Show)
instance A.FromJSON ItchBuildResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 18
		}

data ItchUrlResponse = ItchUrlResponse
	{ itchUrlResponse_url :: !T.Text
	} deriving (Generic, Show)
instance A.FromJSON ItchUrlResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 16
		}

newtype ItchGameId = ItchGameId Word64 deriving (Eq, Ord, Generic, Show, S.Serialize, A.FromJSON, Hashable)
data ItchGame = ItchGame
	{ itchGame_id :: {-# UNPACK #-} !ItchGameId
	, itchGame_title :: !T.Text
	, itchGame_url :: !T.Text
	, itchGame_cover_url :: !(Maybe T.Text)
	, itchGame_user :: !ItchUser
	, itchGame_classification :: !T.Text
	, itchGame_can_be_bought :: !Bool
	, itchGame_in_press_system :: !Bool
	, itchGame_short_text :: !(Maybe T.Text)
	, itchGame_has_demo :: !Bool
	, itchGame_min_price :: {-# UNPACK #-} !Int
	, itchGame_p_windows :: !Bool
	, itchGame_p_linux :: !Bool
	, itchGame_p_osx :: !Bool
	, itchGame_p_android :: !Bool
	} deriving (Generic, Show)
instance S.Serialize ItchGame
instance A.FromJSON ItchGame where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 9
		}

data ItchGameShort = ItchGameShort
	{ itchGameShort_id :: {-# UNPACK #-} !ItchGameId
	, itchGameShort_title :: !T.Text
	, itchGameShort_cover_url :: !(Maybe T.Text)
	} deriving (Generic, Show)
instance S.Serialize ItchGameShort
instance A.FromJSON ItchGameShort where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 14
		}

newtype ItchUploadId = ItchUploadId Word64 deriving (Eq, Ord, Generic, Show, S.Serialize, A.FromJSON, Hashable)
data ItchUpload = ItchUpload
	{ itchUpload_id :: !ItchUploadId
	, itchUpload_display_name :: !(Maybe T.Text)
	, itchUpload_filename :: !T.Text
	, itchUpload_game_id :: {-# UNPACK #-} !ItchGameId
	, itchUpload_demo :: !Bool
	, itchUpload_preorder :: !Bool
	, itchUpload_size :: !Integer
	, itchUpload_p_windows :: !Bool
	, itchUpload_p_linux :: !Bool
	, itchUpload_p_osx :: !Bool
	, itchUpload_p_android :: !Bool
	} deriving (Generic, Show)
instance S.Serialize ItchUpload
instance A.FromJSON ItchUpload where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 11
		}

newtype ItchBuildId = ItchBuildId Word64 deriving (Eq, Ord, Generic, Show, S.Serialize, A.FromJSON, Hashable)
data ItchBuild = ItchBuild
	{ itchBuild_id :: !ItchBuildId
	, itchBuild_version :: !Word64
	, itchBuild_user_version :: !(Maybe T.Text)
	, itchBuild_created_at :: !T.Text
	, itchBuild_updated_at :: !T.Text
	} deriving (Generic, Show)
instance S.Serialize ItchBuild
instance A.FromJSON ItchBuild where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 10
		}

newtype ItchDownloadKeyId = ItchDownloadKeyId Word64 deriving (Eq, Ord, Generic, Show, S.Serialize, A.FromJSON, Hashable)
