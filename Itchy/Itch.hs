{-|
Module: Itchy.Itch
Description: Itch API
-}

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Itchy.Itch
	( ItchApi()
	, newItchApi
	, itchJwtMe
	, itchGetUser
	, itchGetGame
	, itchGetGameUploads
	, itchDownloadUpload
	, itchSearchGame
	, ItchUserId(..)
	, ItchUser(..)
	, ItchGameId(..)
	, ItchGame(..)
	, ItchUploadId(..)
	, ItchUpload(..)
	, ItchDownloadKeyId(..)
	) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Serialize as S
import Data.Serialize.Text()
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

itchGetGame :: ItchApi -> ItchGameId -> IO (Either [T.Text] ItchGame)
itchGetGame api (ItchGameId gameId) = do
	response <- itchRequest api ("/game/" <> fromString (show gameId)) []
	case response of
		ItchGameResponse
			{ itchGameResponse_game = Just game
			} -> return $ Right game
		ItchGameResponse
			{ itchGameResponse_errors = Just errors
			} -> return $ Left errors
		_ -> return $ Left []

itchGetGameUploads :: ItchApi -> ItchGameId -> Maybe ItchDownloadKeyId -> IO (Either [T.Text] [ItchUpload])
itchGetGameUploads api (ItchGameId gameId) maybeDownloadKeyId = do
	let path = case maybeDownloadKeyId of
		Just (ItchDownloadKeyId downloadKeyId) -> "/download-key/" <> fromString (show downloadKeyId) <> "/uploads"
		Nothing -> "/game/" <> fromString (show gameId) <> "/uploads"
	response <- itchRequest api path []
	case response of
		ItchUploadsResponse
			{ itchUploadsResponse_uploads = Just uploads
			} -> return $ Right uploads
		ItchUploadsResponse
			{ itchUploadsResponse_errors = Just errors
			} -> return $ Left errors
		_ -> return $ Left []

itchDownloadUpload :: ItchApi -> ItchUploadId -> Maybe ItchDownloadKeyId -> IO T.Text
itchDownloadUpload api (ItchUploadId uploadId) maybeDownloadKeyId = itchUrlResponse_url <$> itchRequest api ("/upload/" <> T.encodeUtf8 (T.pack $ show uploadId) <> "/download") params where
	params = case maybeDownloadKeyId of
		Just (ItchDownloadKeyId downloadKeyId) -> [("download_key_id", Just $ T.encodeUtf8 $ T.pack $ show downloadKeyId)]
		Nothing -> []

itchSearchGame :: ItchApi -> T.Text -> IO [ItchGame]
itchSearchGame api query = itchGamesResponse_games <$> itchRequest api "/search/games" [("query", Just (T.encodeUtf8 query))]

newtype ItchUserId = ItchUserId Word64 deriving (Generic, Show, S.Serialize, A.FromJSON)
data ItchUser = ItchUser
	{ itchUser_id :: !ItchUserId
	, itchUser_username :: !T.Text
	, itchUser_url :: !T.Text
	, itchUser_cover_url :: !T.Text
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
	, itchGameResponse_errors :: !(Maybe [T.Text])
	} deriving (Generic, Show)
instance A.FromJSON ItchGameResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 17
		}

data ItchGamesResponse = ItchGamesResponse
	{ itchGamesResponse_games :: [ItchGame]
	} deriving (Generic, Show)
instance A.FromJSON ItchGamesResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 18
		}

data ItchUploadsResponse = ItchUploadsResponse
	{ itchUploadsResponse_uploads :: !(Maybe [ItchUpload])
	, itchUploadsResponse_errors :: !(Maybe [T.Text])
	} deriving (Generic, Show)
instance A.FromJSON ItchUploadsResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 20
		}

data ItchUrlResponse = ItchUrlResponse
	{ itchUrlResponse_url :: !T.Text
	} deriving (Generic, Show)
instance A.FromJSON ItchUrlResponse where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 16
		}

newtype ItchGameId = ItchGameId Word64 deriving (Eq, Ord, Generic, Show, S.Serialize, A.FromJSON)
data ItchGame = ItchGame
	{ itchGame_id :: !ItchGameId
	, itchGame_title :: !T.Text
	, itchGame_url :: !T.Text
	, itchGame_cover_url :: !(Maybe T.Text)
	, itchGame_user :: !ItchUser
	, itchGame_classification :: !T.Text
	, itchGame_can_be_bought :: !Bool
	, itchGame_in_press_system :: !Bool
	, itchGame_short_text :: !(Maybe T.Text)
	, itchGame_has_demo :: !Bool
	, itchGame_min_price :: !Int
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

newtype ItchUploadId = ItchUploadId Word64 deriving (Eq, Ord, Generic, Show, S.Serialize, A.FromJSON)
data ItchUpload = ItchUpload
	{ itchUpload_id :: !ItchUploadId
	, itchUpload_display_name :: !(Maybe T.Text)
	, itchUpload_filename :: !T.Text
	, itchUpload_game_id :: !Int
	, itchUpload_demo :: !Bool
	, itchUpload_preorder :: !Bool
	, itchUpload_size :: !Int
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

newtype ItchDownloadKeyId = ItchDownloadKeyId Word64 deriving (Eq, Ord, Generic, Show, S.Serialize, A.FromJSON)
