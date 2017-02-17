{-|
Module: Itchy.Routes
Description: Web app routes
-}

{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, RankNTypes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

module Itchy.Routes
	( App(..)
	) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteArray.Encoding as BA
import Data.Maybe
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Word
import qualified Network.HTTP.Types as HT
import Numeric
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as H(renderHtml)
import qualified Wai.Routes as W
import qualified Web.Cookie as W

import Itchy.ItchInvestigator
import Itchy.Itch
import Itchy.ItchCache
import Itchy.Static

data App = App
	{ appItchApi :: !ItchApi
	, appItchCache :: !ItchCache
	, appItchInvestigator :: !ItchInvestigator
	}

W.mkRoute "App" [W.parseRoutes|
/ DashboardR GET
/reports ReportsR GET
/games GamesR GET
/game/#Word64 GameR GET
/upload/#Word64 UploadR GET
/investigateUpload/#Word64 InvestigateUploadR POST
/auth AuthR POST
|]

getDashboardR :: W.Handler App
getDashboardR = W.runHandlerM $ do
	showRoute <- W.showRouteSub
	page "Dashboard" [("Dashboard", DashboardR)] $ do
		a ! href (toValue $ showRoute ReportsR) $ "Reports"

getReportsR :: W.Handler App
getReportsR = W.runHandlerM $ page "Reports" [("Dashboard", DashboardR), ("Reports", ReportsR)] $ do
	mempty

getGamesR :: W.Handler App
getGamesR = W.runHandlerM $ do
	page "Games" [("Dashboard", DashboardR), ("Games", GamesR)] $ do
		mempty

getGameR :: Word64 -> W.Handler App
getGameR gameId = W.runHandlerM $ do
	App
		{ appItchCache = itchCache
		} <- W.sub
	showRoute <- W.showRouteSub

	maybeGameWithUploads <- liftIO $ itchCacheGetGame itchCache (ItchGameId gameId)

	case maybeGameWithUploads of
		Just (ItchGame
			{ itchGame_title = gameTitle
			, itchGame_cover_url = maybeGameCoverUrl
			, itchGame_user = ItchUser
				{ itchUser_username = creatorUserName
				}
			, itchGame_can_be_bought = gameCanBeBought
			, itchGame_in_press_system = gameInPressSystem
			, itchGame_short_text = fromMaybe "" -> gameShortText
			, itchGame_has_demo = gameHasDemo
			, itchGame_min_price = ((* (0.01 :: Float)) . fromIntegral) -> gameMinPrice
			, itchGame_p_windows = gameWindows
			, itchGame_p_linux = gameLinux
			, itchGame_p_osx = gameMacOS
			, itchGame_p_android = gameAndroid
			}, gameUploads) -> do
			let gameByAuthor = gameTitle <> " by " <> creatorUserName
			page gameByAuthor [("Dashboard", DashboardR), ("Games", GamesR), (gameByAuthor, GameR gameId)] $ H.div ! class_ "game_info" $ do
				case maybeGameCoverUrl of
					Just coverUrl -> img ! class_ "cover" ! src (toValue coverUrl)
					Nothing -> mempty
				p $ toHtml gameShortText
				p $ "Platforms: "
					<> (if gameWindows then H.span ! class_ "tag" $ "windows" else mempty)
					<> (if gameLinux then H.span ! class_ "tag" $ "linux" else mempty)
					<> (if gameMacOS then H.span ! class_ "tag" $ "macos" else mempty)
					<> (if gameAndroid then H.span ! class_ "tag" $ "android" else mempty)
				p $ if gameHasDemo then "Has demo" else "No demo"
				p $
					if gameCanBeBought then
						if gameMinPrice <= 0 then "Free, donations allowed"
						else "Minimum price $" <> toHtml (T.pack $ show gameMinPrice)
					else "Free, payments disabled"
				p $ if gameInPressSystem then "Opted into itch.io press system" else "Did not opted into itch.io press system"
				h2 "Uploads"
				table $ do
					tr $ do
						th "Display name"
						th "File name"
						th "Size"
						th "Tags"
					forM_ gameUploads $ \ItchUpload
						{ itchUpload_id = ItchUploadId uploadId
						, itchUpload_display_name = fromMaybe "" -> uploadDisplayName
						, itchUpload_filename = uploadFileName
						, itchUpload_demo = uploadDemo
						, itchUpload_preorder = uploadPreorder
						, itchUpload_size = uploadSize
						, itchUpload_p_windows = uploadWindows
						, itchUpload_p_linux = uploadLinux
						, itchUpload_p_osx = uploadMacOS
						, itchUpload_p_android = uploadAndroid
						} -> H.tr ! class_ "upload" $ do
						H.td ! class_ "name" $ a ! href (toValue $ showRoute $ UploadR uploadId) $ toHtml uploadDisplayName
						H.td ! class_ "filename" $ toHtml uploadFileName
						H.td $
							if uploadSize < 2 * 1024 then toHtml (show uploadSize) <> " b"
							else if uploadSize < 2 * 1024 * 1024 then toHtml (showFFloat (Just 1) (fromIntegral uploadSize / 1024 :: Float) "") <> " Kb"
							else if uploadSize < 2 * 1024 * 1024 * 1024 then toHtml (showFFloat (Just 1) (fromIntegral uploadSize / (1024 * 1024) :: Float) "") <> " Mb"
							else toHtml (showFFloat (Just 1) (fromIntegral uploadSize / (1024 * 1024 * 1024) :: Float) "") <> " Gb"
						H.td $ do
							if uploadWindows then H.span ! class_ "tag" $ "windows" else mempty
							if uploadLinux then H.span ! class_ "tag" $ "linux" else mempty
							if uploadMacOS then H.span ! class_ "tag" $ "macos" else mempty
							if uploadAndroid then H.span ! class_ "tag" $ "android" else mempty
							if uploadDemo then H.span ! class_ "tag" $ "demo" else mempty
							if uploadPreorder then H.span ! class_ "tag" $ "preorder" else mempty
		Nothing -> do
			let gameByAuthor = "unknown"
			page gameByAuthor [("Dashboard", DashboardR), ("Games", GamesR), (gameByAuthor, GameR gameId)] $ H.p "Information about this game is not cached yet."

getUploadR :: Word64 -> W.Handler App
getUploadR (ItchUploadId -> uploadId) = W.runHandlerM $ do
	App
		{ appItchApi = itchApi
		} <- W.sub
	maybeDownloadKeyId <- W.getParam "downloadKey"
	url <- liftIO $ itchDownloadUpload itchApi uploadId (ItchDownloadKeyId . read . T.unpack <$> maybeDownloadKeyId)
	W.header "Location" $ T.encodeUtf8 url
	W.status HT.seeOther303

postInvestigateUploadR :: Word64 -> W.Handler App
postInvestigateUploadR (ItchUploadId -> uploadId) = W.runHandlerM $ do
	App
		{ appItchCache = itchCache
		, appItchInvestigator = itchInvestigator
		} <- W.sub
	maybeUpload <- liftIO $ itchCacheGetUpload itchCache uploadId
	case maybeUpload of
		Just ItchUpload
			{ itchUpload_filename = uploadFileName
			} -> do
			liftIO $ investigateItchUpload itchInvestigator uploadId uploadFileName $ itchCachePutReport itchCache uploadId
			W.status HT.noContent204
		Nothing -> W.status HT.notFound404

postAuthR :: W.Handler App
postAuthR = W.runHandlerM $ do
	App
		{ appItchApi = itchApi
		} <- W.sub
	maybeToken <- W.getPostParam "token"
	maybeUser <- case maybeToken of
		Just token -> liftIO $ Just <$> itchJwtMe itchApi token
		Nothing -> return Nothing
	case maybeUser of
		Just user -> do
			W.setCookie W.def
				{ W.setCookieName = "user"
				, W.setCookieValue = BA.convertToBase BA.Base64URLUnpadded $ S.encode user
				}
			showRoute <- W.showRouteSub
			W.header "Location" (T.encodeUtf8 $ showRoute DashboardR)
			W.status HT.seeOther303
		Nothing -> W.status HT.notFound404

page :: W.RenderRoute master => T.Text -> [(T.Text, W.Route App)] -> Html -> W.HandlerM App master ()
page titleText pieces bodyHtml = do
	W.header HT.hCacheControl "public, max-age=1"
	maybeUserCookie <- W.getCookie "user"
	let maybeUser = case maybeUserCookie of
		Just (BA.convertFromBase BA.Base64URLUnpadded . T.encodeUtf8 -> Right (S.decode -> Right user)) -> Just user
		_ -> Nothing
	showRoute <- W.showRouteSub
	W.html $ TL.toStrict $ H.renderHtml $ docTypeHtml $ do
		H.head $ do
			meta ! charset "utf-8"
			meta ! name "robots" ! content "index,follow"
			link ! rel "stylesheet" ! href [staticPath|static-stylus/itchy.css|]
			link ! rel "icon" ! type_ "image/png" ! href [staticPath|static/itchio.svg|]
			script ! src [staticPath|static/jquery-2.1.4.min.js|] $ mempty
			script ! src [staticPath|static-js/itchy.js|] $ mempty
			H.title $ toHtml $ titleText <> " - " <> "itch.io Developer's Sanity Keeper"
		body $ do
			H.div ! class_ "header" $ do
				H.div ! class_ "pieces" $ forM_ pieces $ \(pieceName, pieceRoute) -> do
					void "/ "
					a ! href (toValue $ showRoute pieceRoute) $ toHtml pieceName
					void " "
				case maybeUser of
					Just ItchUser
						{ itchUser_username = userName
						, itchUser_url = userUrl
						, itchUser_cover_url = userCoverUrl
						} -> H.div ! class_ "user" $ do
						a ! href (toValue userUrl) $ do
							img ! src (toValue userCoverUrl)
							toHtml userName
					Nothing -> mempty
			h1 $ toHtml titleText
			bodyHtml
