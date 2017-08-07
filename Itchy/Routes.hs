{-|
Module: Itchy.Routes
Description: Web app routes
License: MIT
-}

{-# LANGUAGE BangPatterns, LambdaCase, MultiParamTypeClasses, OverloadedLists, OverloadedStrings, QuasiQuotes, RankNTypes, TemplateHaskell, TypeFamilies, ViewPatterns #-}

module Itchy.Routes
	( App(..)
	) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteArray.Encoding as BA
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.List
import qualified Data.Map.Strict as M
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
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5((!))
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
import Itchy.Report
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
/upload/#Word64/download UploadDownloadR GET
/investigateUpload/#Word64 InvestigateUploadR POST
/auth AuthR POST
/search SearchR GET
/gamebyurl GameByUrlR GET
|]

getHomeR :: W.Handler App
getHomeR = W.runHandlerM $ do
	showRoute <- W.showRouteSub
	loc <- getLocalization
	page (locHome loc) [(locHome loc, HomeR)] $ do
		H.p $ H.toHtml $ locWelcome loc
		H.form ! A.method "GET" ! A.action (H.toValue $ showRoute SearchR) $ do
			H.label ! A.type_ "text" ! A.for "searchtext" $ H.toHtml $ locSearchGameByName loc
			H.br
			H.input ! A.type_ "text" ! A.id "searchtext" ! A.name "s"
			H.input ! A.type_ "submit" ! A.value (H.toValue $ locSearch loc)
		H.form ! A.method "GET" ! A.action (H.toValue $ showRoute GameByUrlR) $ do
			H.label ! A.type_ "text" ! A.for "urltext" $ H.toHtml $ locGoToGameByUrl loc
			H.br
			H.input ! A.type_ "text" ! A.id "urltext" ! A.name "url" ! A.placeholder "[https://]creator[.itch.io]/game[/]"
			H.input ! A.type_ "submit" ! A.value (H.toValue $ locGo loc)

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
			, itchGame_url = gameUrl
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
					Just coverUrl -> H.img ! A.class_ "cover" ! A.src (H.toValue coverUrl)
					Nothing -> mempty
				H.p $ H.toHtml $ locLink loc gameUrl
				H.p $ H.toHtml $ locDescription loc gameShortText
				H.p $ H.toHtml (locPlatforms loc) <> ": "
					<> (if gameWindows then H.span ! A.class_ "tag" $ "windows" else mempty)
					<> (if gameLinux then H.span ! A.class_ "tag" $ "linux" else mempty)
					<> (if gameMacOS then H.span ! A.class_ "tag" $ "macos" else mempty)
					<> (if gameAndroid then H.span ! A.class_ "tag" $ "android" else mempty)
				H.p $ H.toHtml $ if gameHasDemo then locHasDemo loc else locNoDemo loc
				H.p $ H.toHtml $
					if gameCanBeBought then
						if gameMinPrice <= 0 then locFreeDonationsAllowed loc
						else locMinimumPrice loc <> ": $" <> T.pack (show gameMinPrice)
					else locFreePaymentsDisabled loc
				H.p $ H.toHtml $ if gameInPressSystem then locOptedInPressSystem loc else locNotOptedInPressSystem loc
				H.h2 $ H.toHtml $ locUploads loc
				H.table $ do
					H.tr $ do
						H.th $ H.toHtml $ locDisplayName loc
						H.th $ H.toHtml $ locFileName loc
						H.th $ H.toHtml $ locSize loc
						H.th $ H.toHtml $ locTags loc
						H.th "Butler"
						H.th $ H.toHtml $ locReportStatus loc
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
						H.td ! A.class_ "name" $ H.toHtml uploadDisplayName
						H.td ! A.class_ "filename" $ {- a ! A.href (H.toValue $ showRoute $ UploadR uploadId) $ -} H.toHtml uploadFileName
						H.td $ H.toHtml $ locSizeInBytes loc uploadSize
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
							ItchQueuedInvestigation n -> H.toHtml $ locInvestigationQueued loc $ n + 1
							ItchProcessingInvestigation -> H.toHtml $ locInvestigationProcessing loc
							ItchInvestigation
								{ itchInvestigationMaybeReport = maybeReport
								, itchInvestigationTime = t
								} -> do
								if isJust maybeReport
									then H.a ! A.href (H.toValue $ showRoute $ UploadR uploadId) ! A.target "_blank" $ H.toHtml $ locInvestigationSucceeded loc
									else H.toHtml $ locInvestigationFailed loc
								when (t < reinvestigateTimeCutoff) $ H.form ! A.class_ "formreprocess" ! A.action (H.toValue $ showRoute $ InvestigateUploadR uploadId) ! A.method "POST" $
									H.input ! A.type_ "submit" ! A.value (H.toValue $ locReinvestigate loc)
				H.h2 $ H.toHtml $ locReport loc
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
						H.th $ H.toHtml $ locRecordSeverity loc
						H.th ! A.class_ "scope" $ H.toHtml $ locRecordScope loc
						H.th ! A.class_ "name" $ H.toHtml $ locRecordName loc
						H.th ! A.class_ "name" $ H.toHtml $ locRecordMessage loc
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
							H.td ! A.class_ "status" $ H.div $ H.toHtml ttl
							H.td $ H.toHtml $ case scope of
								ProjectScope -> locScopeProject loc
								UploadGroupScope uploadGroup -> locScopeUploadGroup loc uploadGroup
								UploadScope uploadId -> locScopeUpload loc (uploadName uploadId)
								EntryScope uploadId entryPath -> locScopeEntry loc (uploadName uploadId) (T.intercalate "/" entryPath)
							H.td $ H.div ! A.class_ "record" $ H.toHtml name
							H.td $ unless (message == RichText []) $
								H.div ! A.class_ "message" $ H.toHtml message
		Nothing -> do
			let gameByAuthor = locUnknownGame loc
			W.header "Refresh" "3"
			page gameByAuthor [(locHome loc, HomeR), (gameByAuthor, GameR gameId)] $ do
				H.p $ H.toHtml $ locGameNotCached loc
				H.p $ H.a ! A.href (H.toValue $ showRoute $ GameR gameId) $ H.toHtml $ locRefresh loc

getUploadR :: Word64 -> W.Handler App
getUploadR uploadId = W.runHandlerM $ do
	App
		{ appItchCache = itchCache
		, appItchInvestigator = itchInvestigator
		} <- W.sub
	loc <- getLocalization
	maybeUpload <- liftIO $ itchCacheGetUpload itchCache $ ItchUploadId uploadId
	case maybeUpload of
		Just (ItchUpload
			{ itchUpload_filename = uploadFileName
			, itchUpload_game_id = ItchGameId gameId
			}, _maybeBuild) -> do
			maybeGameWithUploads <- liftIO $ itchCacheGetGame itchCache $ ItchGameId gameId
			case maybeGameWithUploads of
				Just (ItchGame
					{ itchGame_title = gameTitle
					, itchGame_user = ItchUser
						{ itchUser_username = creatorUserName
						}
					}, _) -> do
					let gameByAuthor = locGameByAuthor loc gameTitle creatorUserName
					investigation <- liftIO $ investigateItchUpload itchInvestigator (ItchUploadId uploadId) uploadFileName False
					case investigation of
						ItchInvestigation
							{ itchInvestigationMaybeReport = maybeReport
							} -> page uploadFileName [(locHome loc, HomeR), (gameByAuthor, GameR gameId), (uploadFileName, UploadR uploadId)] $ do
							case maybeReport of
								Just Report
									{ report_unpack = ReportUnpack_succeeded rootEntries
									} -> H.table ! A.class_ "entries" $ do
									H.tr $ do
										H.th $ H.toHtml $ locFileName loc
										H.th $ H.toHtml $ locSize loc
										H.th $ H.toHtml $ locAccessMode loc
										H.th $ H.toHtml $ locTags loc
									let
										tag = H.span ! A.class_ "tag"
										tagArch = \case
											ReportArch_unknown -> mempty
											ReportArch_x86 -> tag "x86"
											ReportArch_x64 -> tag "x64"
										printEntries level = mapM_ (printEntry level) . M.toAscList
										printEntry level (entryName, entry) = do
											H.tr $ do
												H.td ! A.style ("padding-left: " <> (H.toValue $ 5 + level * 20) <> "px") $ H.toHtml entryName
												H.td $ case entry of
													ReportEntry_file
														{ reportEntry_size = entrySize
														} -> H.toHtml $ locSizeInBytes loc $ toInteger entrySize
													_ -> mempty
												H.td $ do
													let entryMode = reportEntry_mode entry
													let isDir = case entry of
														ReportEntry_directory {} -> True
														_ -> False
													tag $ H.toHtml $
														(if isDir then 'd' else '.') :
														(if (entryMode .&. 0x100) > 0 then 'r' else '.') :
														(if (entryMode .&. 0x80) > 0 then 'w' else '.') :
														(if (entryMode .&. 0x40) > 0 then 'x' else '.') :
														(if (entryMode .&. 0x20) > 0 then 'r' else '.') :
														(if (entryMode .&. 0x10) > 0 then 'w' else '.') :
														(if (entryMode .&. 0x8) > 0 then 'x' else '.') :
														(if (entryMode .&. 0x4) > 0 then 'r' else '.') :
														(if (entryMode .&. 0x2) > 0 then 'w' else '.') :
														(if (entryMode .&. 0x1) > 0 then 'x' else '.') : []
												H.td $ case entry of
													ReportEntry_unknown {} -> mempty
													ReportEntry_file
														{ reportEntry_parses = entryParses
														} -> forM_ entryParses $ \case
														ReportParse_itchToml {} -> tag ".itch.toml"
														ReportParse_binaryPe ReportBinaryPe
															{ reportBinaryPe_arch = arch
															, reportBinaryPe_isCLR = isCLR
															} -> do
															tag "PE"
															when isCLR $ tag "CLR"
															tagArch arch
														ReportParse_binaryElf ReportBinaryElf
															{ reportBinaryElf_arch = arch
															} -> do
															tag "ELF"
															tagArch arch
														ReportParse_binaryMachO ReportBinaryMachO
															{ reportBinaryMachO_binaries = subBinaries
															} -> do
															tag "Mach-O"
															forM_ subBinaries $ \ReportMachOSubBinary
																{ reportMachoSubBinary_arch = arch
																} -> tagArch arch
														ReportParse_archive {} -> mempty
														ReportParse_msi {} -> tag "msi"
													ReportEntry_directory {} -> mempty
													ReportEntry_symlink
														{ reportEntry_link = entryLink
														} -> do
														tag $ H.toHtml $ locSymlink loc
														H.toHtml entryLink
											case entry of
												ReportEntry_file
													{ reportEntry_parses = parses
													} -> forM_ parses $ \case
													ReportParse_archive ReportArchive
														{ reportArchive_entries = entryEntries
														} -> printEntries (level + 1) entryEntries
													ReportParse_msi ReportMsi
														{ reportMsi_entries = entryEntries
														} -> printEntries (level + 1) entryEntries
													_ -> mempty
												ReportEntry_directory
													{ reportEntry_entries = entryEntries
													} -> printEntries (level + 1) entryEntries
												_ -> mempty
									printEntries (0 :: Int) rootEntries
								_ -> return ()
						_ -> W.status HT.notFound404
				Nothing -> W.status HT.notFound404
		Nothing -> W.status HT.notFound404

getUploadDownloadR :: Word64 -> W.Handler App
getUploadDownloadR (ItchUploadId -> uploadId) = W.runHandlerM $ do
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
	showRoute <- W.showRouteSub
	maybeUpload <- liftIO $ itchCacheGetUpload itchCache uploadId
	case maybeUpload of
		Just (ItchUpload
			{ itchUpload_filename = uploadFileName
			, itchUpload_game_id = ItchGameId gameId
			}, _maybeBuild) -> do
			void $ liftIO $ investigateItchUpload itchInvestigator uploadId uploadFileName True
			W.header "Location" (T.encodeUtf8 $ showRoute $ GameR gameId)
			W.status HT.seeOther303
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
	Right games <- if T.null searchText then return $ Right V.empty else liftIO $ itchSearchGame itchApi searchText
	page (locSearch loc) [(locHome loc, HomeR), (locSearch loc, SearchR)] $ do
		H.form ! A.method "GET" ! A.action (H.toValue $ showRoute SearchR) $ do
			H.input ! A.type_ "text" ! A.name "s" ! A.value (H.toValue searchText)
			H.input ! A.type_ "submit" ! A.value (H.toValue $ locSearch loc)
		H.div ! A.class_ "searchresults" $ forM_ games $ \ItchGameShort
			{ itchGameShort_id = ItchGameId gameId
			, itchGameShort_title = gameTitle
			, itchGameShort_cover_url = maybeGameCoverUrl
			} -> H.a ! A.class_ "game" ! A.href (H.toValue $ showRoute $ GameR gameId) ! A.target "_blank" $ do
			case maybeGameCoverUrl of
				Just gameCoverUrl -> H.img ! A.src (H.toValue gameCoverUrl)
				Nothing -> mempty
			H.span $ H.toHtml gameTitle

getGameByUrlR :: W.Handler App
getGameByUrlR = W.runHandlerM $ do
	App
		{ appItchApi = itchApi
		} <- W.sub
	showRoute <- W.showRouteSub
	searchText <- fromMaybe "" <$> W.getParam "url"
	-- strip various stuff, and parse
	let
		pref p s = fromMaybe s $ T.stripPrefix p s
		suf q s = fromMaybe s $ T.stripSuffix q s
	case T.splitOn "/" $ T.replace ".itch.io/" "/" $ suf "/" $ pref "https://" $ pref "http://" $ searchText of
		[creator, game] -> do
			maybeGameId <- liftIO $ itchGameGetByUrl itchApi creator game
			case maybeGameId of
				Just (ItchGameId gameId) -> do
					W.header "Location" $ T.encodeUtf8 $ showRoute $ GameR gameId
					W.status HT.seeOther303
				Nothing -> W.status HT.notFound404
		_ -> W.status HT.notFound404

page :: W.RenderRoute master => T.Text -> [(T.Text, W.Route App)] -> H.Html -> W.HandlerM App master ()
page titleText pieces bodyHtml = do
	W.header HT.hCacheControl "public, max-age=1"
	maybeUserCookie <- W.getCookie "user"
	let maybeUser = case maybeUserCookie of
		Just (BA.convertFromBase BA.Base64URLUnpadded . T.encodeUtf8 -> Right (S.decode -> Right user)) -> Just user
		_ -> Nothing
	showRoute <- W.showRouteSub
	W.html $ TL.toStrict $ H.renderHtml $ H.docTypeHtml $ do
		H.head $ do
			H.meta ! A.charset "utf-8"
			H.meta ! A.name "robots" ! A.content "index,follow"
			H.link ! A.rel "stylesheet" ! A.href [staticPath|static-stylus/itchy.css|]
			H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href [staticPath|static/itchio.svg|]
			H.script ! A.src [staticPath|static/jquery-2.1.4.min.js|] $ mempty
			H.script ! A.src [staticPath|static-js/itchy.js|] $ mempty
			H.title $ H.toHtml $ titleText <> " - " <> "itch.io Developer's Sanity Keeper"
		H.body $ do
			H.div ! A.class_ "header" $ do
				H.div ! A.class_ "pieces" $ forM_ pieces $ \(pieceName, pieceRoute) -> do
					void "/ "
					H.a ! A.href (H.toValue $ showRoute pieceRoute) $ H.toHtml pieceName
					void " "
				case maybeUser of
					Just ItchUser
						{ itchUser_username = userName
						, itchUser_url = userUrl
						, itchUser_cover_url = userMaybeCoverUrl
						} -> H.div ! A.class_ "user" $ do
						H.a ! A.href (H.toValue userUrl) $ do
							case userMaybeCoverUrl of
								Just userCoverUrl -> H.img ! A.src (H.toValue userCoverUrl)
								Nothing -> mempty
							H.toHtml userName
					Nothing -> mempty
			H.h1 $ H.toHtml titleText
			bodyHtml
			H.div ! A.class_ "footer" $ do
				H.span ! A.class_ "localizations" $ forM_ localizations $ \(locale, localization) ->
					H.a ! A.href ("?locale=" <> H.toValue locale) $ H.toHtml $ locLanguageName localization
				H.span " | "
				H.a ! A.href "https://github.com/quyse/itchy" ! A.target "_blank" $ "Github"
