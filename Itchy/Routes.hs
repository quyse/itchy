{-|
Module: Itchy.Routes
Description: Web app routes
-}

{-# LANGUAGE BangPatterns, MultiParamTypeClasses, OverloadedLists, OverloadedStrings, QuasiQuotes, RankNTypes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

module Itchy.Routes
	( App(..)
	) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteArray.Encoding as BA
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Word
import Foreign.C.Types
import qualified Network.HTTP.Types as HT
import System.Posix.Time
import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as H(renderHtml)
import qualified Wai.Routes as W
import qualified Web.Cookie as W

import Itchy.ItchInvestigator
import Itchy.Itch
import Itchy.ItchCache
import Itchy.Localization
import Itchy.Localization.En
import Itchy.Localization.RichText
import Itchy.Localization.Ru
import Itchy.Report.Analysis
import Itchy.Report.Record
import Itchy.Static

data App = App
	{ appItchApi :: !ItchApi
	, appItchCache :: !ItchCache
	, appItchInvestigator :: !ItchInvestigator
	, appItchInvestigationStalePeriod :: {-# UNPACK #-} !Int64
	}

localizations :: [(T.Text, Localization)]
localizations =
	[ ("en", localizationEn)
	, ("ru", localizationRu)
	]

getLocalization :: W.HandlerM App master Localization
getLocalization = do
	maybeNewLocale <- W.getParam "locale"
	locale <- case maybeNewLocale of
		Just newLocale -> do
			W.setCookie W.def
				{ W.setCookieName ="locale"
				, W.setCookieValue = T.encodeUtf8 newLocale
				, W.setCookiePath = Just "/"
				, W.setCookieMaxAge = Just $ 365 * 24 * 3600
				}
			return newLocale
		Nothing -> fromMaybe "en" <$> W.getCookie "locale"
	return $ fromMaybe localizationEn $ lookup locale localizations

W.mkRoute "App" [W.parseRoutes|
/ HomeR GET
/game/#Word64 GameR GET
/upload/#Word64 UploadR GET
/investigateUpload/#Word64 InvestigateUploadR POST
/auth AuthR POST
/search SearchR GET
|]

getHomeR :: W.Handler App
getHomeR = W.runHandlerM $ do
	showRoute <- W.showRouteSub
	loc <- getLocalization
	page (locHome loc) [(locHome loc, HomeR)] $
		H.form ! A.method "GET" ! A.action (H.toValue $ showRoute SearchR) $ do
			H.input ! A.type_ "text" ! A.name "s"
			H.input ! A.type_ "submit" ! A.value (H.toValue $ locSearch loc)

getGameR :: Word64 -> W.Handler App
getGameR gameId = W.runHandlerM $ do
	App
		{ appItchCache = itchCache
		, appItchInvestigator = itchInvestigator
		, appItchInvestigationStalePeriod = investigationStalePeriod
		} <- W.sub
	showRoute <- W.showRouteSub
	loc <- getLocalization

	maybeGameWithUploads <- liftIO $ itchCacheGetGame itchCache (ItchGameId gameId)

	case maybeGameWithUploads of
		Just (game@ItchGame
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
			let gameByAuthor = locGameByAuthor loc gameTitle creatorUserName
			let uploadsById = HM.fromList $ V.toList $ flip fmap gameUploads $ \(upload@ItchUpload
				{ itchUpload_id = uploadId
				}, _maybeBuild) -> (uploadId, upload)
			let uploadName uploadId = case HM.lookup uploadId uploadsById of
				Just ItchUpload
					{ itchUpload_display_name = maybeDisplayName
					, itchUpload_filename = fileName
					} -> Just $ fromMaybe fileName maybeDisplayName
				Nothing -> Nothing
			investigations <- liftIO $ forM gameUploads $ \(ItchUpload
				{ itchUpload_id = uploadId
				, itchUpload_filename = uploadFileName
				}, _maybeBuild) -> investigateItchUpload itchInvestigator uploadId uploadFileName False
			CTime currentTime <- liftIO epochTime
			let reinvestigateTimeCutoff = currentTime - investigationStalePeriod
			page gameByAuthor [(locHome loc, HomeR), (gameByAuthor, GameR gameId)] $ H.div ! A.class_ "game_info" $ do
				case maybeGameCoverUrl of
					Just coverUrl -> img ! A.class_ "cover" ! A.src (toValue coverUrl)
					Nothing -> mempty
				p $ toHtml $ locDescription loc gameShortText
				p $ toHtml (locPlatforms loc) <> ": "
					<> (if gameWindows then H.span ! A.class_ "tag" $ "windows" else mempty)
					<> (if gameLinux then H.span ! A.class_ "tag" $ "linux" else mempty)
					<> (if gameMacOS then H.span ! A.class_ "tag" $ "macos" else mempty)
					<> (if gameAndroid then H.span ! A.class_ "tag" $ "android" else mempty)
				p $ toHtml $ if gameHasDemo then locHasDemo loc else locNoDemo loc
				p $ toHtml $
					if gameCanBeBought then
						if gameMinPrice <= 0 then locFreeDonationsAllowed loc
						else locMinimumPrice loc <> ": $" <> T.pack (show gameMinPrice)
					else locFreePaymentsDisabled loc
				p $ toHtml $ if gameInPressSystem then locOptedInPressSystem loc else locNotOptedInPressSystem loc
				h2 $ toHtml $ locUploads loc
				table $ do
					tr $ do
						th $ toHtml $ locDisplayName loc
						th $ toHtml $ locFileName loc
						th $ toHtml $ locSize loc
						th $ toHtml $ locTags loc
						th "Butler"
						th $ toHtml $ locReport loc
					forM_ (V.zip gameUploads investigations) $ \((ItchUpload
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
						}, maybeBuild), investigation) -> H.tr ! A.class_ "upload" $ do
						H.td ! A.class_ "name" $ toHtml uploadDisplayName
						H.td ! A.class_ "filename" $ {- a ! A.href (toValue $ showRoute $ UploadR uploadId) $ -} toHtml uploadFileName
						H.td $ toHtml $ locSizeInBytes loc uploadSize
						H.td $ do
							if uploadWindows then H.span ! A.class_ "tag" $ "windows" else mempty
							if uploadLinux then H.span ! A.class_ "tag" $ "linux" else mempty
							if uploadMacOS then H.span ! A.class_ "tag" $ "macos" else mempty
							if uploadAndroid then H.span ! A.class_ "tag" $ "android" else mempty
							if uploadDemo then H.span ! A.class_ "tag" $ "demo" else mempty
							if uploadPreorder then H.span ! A.class_ "tag" $ "preorder" else mempty
						H.td $ case maybeBuild of
							Just ItchBuild
								{ itchBuild_version = T.pack . show -> buildVersion
								, itchBuild_user_version = fromMaybe (locNoUserVersion loc) -> buildUserVersion
								} -> H.toHtml $ locBuildVersion loc buildVersion buildUserVersion
							Nothing -> H.toHtml $ locDoesntUseButler loc
						H.td $ case investigation of
							ItchStartedInvestigation -> H.toHtml $ locInvestigationStarted loc
							ItchQueuedInvestigation -> H.toHtml $ locInvestigationQueued loc
							ItchProcessingInvestigation -> H.toHtml $ locInvestigationProcessing loc
							ItchInvestigation
								{ itchInvestigationMaybeReport = maybeReport
								, itchInvestigationTime = t
								} -> do
								H.toHtml $ (if isJust maybeReport then locInvestigationSucceeded else locInvestigationFailed) loc
								when (t < reinvestigateTimeCutoff) $ H.form ! A.class_ "formreprocess" ! A.action (toValue $ showRoute $ InvestigateUploadR uploadId) ! A.method "POST" $
									H.input ! A.type_ "submit" ! A.value (toValue $ locReinvestigate loc)
				h2 $ toHtml $ locReport loc
				let
					reportsCount = foldr (\investigation !c -> case investigation of
						ItchInvestigation {} -> c + 1
						_ -> c
						) 0 investigations
					in when (reportsCount < V.length gameUploads) $ H.p $ (H.toHtml $ locReportNotComplete loc reportsCount (V.length gameUploads)) <> " " <> (H.a ! A.href (H.toValue $ showRoute $ GameR gameId) $ H.toHtml $ locRefresh loc)
				let AnalysisGame
					{ analysisGame_uploads = analysisUploads
					, analysisGame_release = AnalysisUploadGroup
						{ analysisUploadGroup_records = releaseGroupRecords
						}
					, analysisGame_preorder = AnalysisUploadGroup
						{ analysisUploadGroup_records = preorderGroupRecords
						}
					, analysisGame_demo = AnalysisUploadGroup
						{ analysisUploadGroup_records = demoGroupRecords
						}
					, analysisGame_records = gameRecords
					} = analyseGame loc game $ concat $
					flip fmap (V.toList $ V.zip gameUploads investigations) $
					\((u, _), inv) -> case inv of
					ItchInvestigation
						{ itchInvestigationMaybeReport = Just r
						} -> [(u, r)]
					_ -> []

				let uploadsRecords = concat $ Prelude.map analysisUpload_records analysisUploads
				let records = flip sortOn
					(  gameRecords
					<> releaseGroupRecords
					<> preorderGroupRecords
					<> demoGroupRecords
					<> uploadsRecords
					) $ \Record
					{ recordScope = scope
					, recordSeverity = severity
					, recordName = name
					, recordMessage = message
					} -> (severity, scope, name, message)

				H.table $ do
					H.tr $ do
						H.th $ toHtml $ locRecordSeverity loc
						H.th ! A.class_ "scope" $ toHtml $ locRecordScope loc
						H.th ! A.class_ "name" $ toHtml $ locRecordName loc
						H.th ! A.class_ "name" $ toHtml $ locRecordMessage loc
					forM_ records $ \Record
						{ recordScope = scope
						, recordSeverity = severity
						, recordName = name
						, recordMessage = message
						} -> let
						(cls, ttl) = case severity of
							SeverityOk -> ("ok", locSeverityOk loc)
							SeverityInfo -> ("info", locSeverityInfo loc)
							SeverityTip -> ("tip", locSeverityTip loc)
							SeverityWarn -> ("warn", locSeverityWarn loc)
							SeverityBad -> ("bad", locSeverityBad loc)
							SeverityErr -> ("err", locSeverityErr loc)
						in H.tr ! A.class_ cls $ do
							H.td ! A.class_ "status" $ H.div $ toHtml ttl
							H.td $ toHtml $ case scope of
								ProjectScope -> locScopeProject loc
								UploadGroupScope uploadGroup -> locScopeUploadGroup loc uploadGroup
								UploadScope uploadId -> locScopeUpload loc (uploadName uploadId)
								EntryScope uploadId entryPath -> locScopeEntry loc (uploadName uploadId) (T.intercalate "/" entryPath)
							H.td $ H.div ! A.class_ "record" $ toHtml name
							H.td $ unless (message == RichText []) $
								H.div ! A.class_ "message" $ toHtml message
		Nothing -> do
			let gameByAuthor = locUnknownGame loc
			page gameByAuthor [(locHome loc, HomeR), (gameByAuthor, GameR gameId)] $ do
				H.p $ H.toHtml $ locGameNotCached loc
				H.p $ H.a ! A.href (H.toValue $ showRoute $ GameR gameId) $ H.toHtml $ locRefresh loc

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
		Just (ItchUpload
			{ itchUpload_filename = uploadFileName
			}, _maybeBuild) -> do
			void $ liftIO $ investigateItchUpload itchInvestigator uploadId uploadFileName True
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
				, W.setCookiePath = Just "/"
				}
			showRoute <- W.showRouteSub
			W.header "Location" (T.encodeUtf8 $ showRoute HomeR)
			W.status HT.seeOther303
		Nothing -> W.status HT.notFound404

getSearchR :: W.Handler App
getSearchR = W.runHandlerM $ do
	App
		{ appItchApi = itchApi
		} <- W.sub
	showRoute <- W.showRouteSub
	loc <- getLocalization
	searchText <- fromMaybe "" <$> W.getParam "s"
	games <- if T.null searchText then return V.empty else liftIO $ itchSearchGame itchApi searchText
	page (locSearch loc) [(locHome loc, HomeR), (locSearch loc, SearchR)] $ do
		H.form ! A.method "GET" ! A.action (H.toValue $ showRoute SearchR) $ do
			H.input ! A.type_ "text" ! A.name "s"
			H.input ! A.type_ "submit" ! A.value (H.toValue $ locSearch loc)
		H.div ! A.class_ "searchresults" $ forM_ games $ \ItchGameShort
			{ itchGameShort_id = ItchGameId gameId
			, itchGameShort_title = gameTitle
			, itchGameShort_cover_url = maybeGameCoverUrl
			} -> H.a ! A.class_ "game" ! A.href (H.toValue $ showRoute $ GameR gameId) $ do
			case maybeGameCoverUrl of
				Just gameCoverUrl -> H.img ! A.src (H.toValue gameCoverUrl)
				Nothing -> mempty
			H.span $ H.toHtml gameTitle

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
			meta ! A.charset "utf-8"
			meta ! A.name "robots" ! A.content "index,follow"
			link ! A.rel "stylesheet" ! A.href [staticPath|static-stylus/itchy.css|]
			link ! A.rel "icon" ! A.type_ "image/png" ! A.href [staticPath|static/itchio.svg|]
			script ! A.src [staticPath|static/jquery-2.1.4.min.js|] $ mempty
			script ! A.src [staticPath|static-js/itchy.js|] $ mempty
			H.title $ toHtml $ titleText <> " - " <> "itch.io Developer's Sanity Keeper"
		body $ do
			H.div ! A.class_ "header" $ do
				H.div ! A.class_ "pieces" $ forM_ pieces $ \(pieceName, pieceRoute) -> do
					void "/ "
					a ! A.href (toValue $ showRoute pieceRoute) $ toHtml pieceName
					void " "
				case maybeUser of
					Just ItchUser
						{ itchUser_username = userName
						, itchUser_url = userUrl
						, itchUser_cover_url = userMaybeCoverUrl
						} -> H.div ! A.class_ "user" $ do
						a ! A.href (toValue userUrl) $ do
							case userMaybeCoverUrl of
								Just userCoverUrl -> img ! A.src (toValue userCoverUrl)
								Nothing -> mempty
							toHtml userName
					Nothing -> mempty
			h1 $ toHtml titleText
			bodyHtml
			H.div ! A.class_ "footer" $
				H.div ! A.class_ "localizations" $ forM_ localizations $ \(locale, localization) ->
					H.a ! A.href ("?locale=" <> toValue locale) $ toHtml $ locLanguageName localization
