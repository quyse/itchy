{-|
Module: Itchy.Itch
Description: Itch API
-}

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Itchy.Itch
	( ItchApi()
	, newItchApi
	, itchJwtMe
	, itchUser
	, itchGame
	, itchGameUploads
	, itchDownloadUpload
	, itchSearchGame
	, ItchUser(..)
	, ItchGame(..)
	, ItchUpload(..)
	) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Serialize as S
import Data.Serialize.Text()
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

itchUser :: ItchApi -> Int -> IO ItchUser
itchUser api userId = itchRequest api ("/users/" <> (fromString $ show userId)) []

itchGame :: ItchApi -> Int -> IO ItchGame
itchGame api gameId = itchGameResponse_game <$> itchRequest api ("/game/" <> fromString (show gameId)) []

itchGameUploads :: ItchApi -> Int -> Maybe Int -> IO [ItchUpload]
itchGameUploads api gameId maybeDownloadKeyId = itchUploadsResponse_uploads <$> itchRequest api path [] where
	path = case maybeDownloadKeyId of
		Just downloadKey -> "/download-key/" <> fromString (show downloadKey) <> "/uploads"
		Nothing -> "/game/" <> fromString (show gameId) <> "/uploads"

itchDownloadUpload :: ItchApi -> Int -> Maybe Int -> IO T.Text
itchDownloadUpload api uploadId maybeDownloadKeyId = itchUrlResponse_url <$> itchRequest api ("/upload/" <> T.encodeUtf8 (T.pack $ show uploadId) <> "/download") params where
	params = case maybeDownloadKeyId of
		Just downloadKey -> [("download_key_id", Just $ T.encodeUtf8 $ T.pack $ show downloadKey)]
		Nothing -> []

itchSearchGame :: ItchApi -> T.Text -> IO [ItchGame]
itchSearchGame api query = itchGamesResponse_games <$> itchRequest api "/search/games" [("query", Just (T.encodeUtf8 query))]

data ItchUser = ItchUser
	{ itchUser_id :: !Int
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
	{ itchGameResponse_game :: !ItchGame
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
	{ itchUploadsResponse_uploads :: [ItchUpload]
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

data ItchGame = ItchGame
	{ itchGame_id :: !Int
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
instance A.FromJSON ItchGame where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 9
		}

data ItchUpload = ItchUpload
	{ itchUpload_id :: !Int
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
instance A.FromJSON ItchUpload where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 11
		}
