{-|
Module: Itchy.Localization.En
Description: English localization.
License: MIT
-}

{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Itchy.Localization.En
	( localizationEn
	) where

import Data.Maybe
import qualified Data.Text as T
import Numeric

import Itchy.Localization
import Itchy.Localization.RichText
import Itchy.Report
import Itchy.Report.Record

localizationEn :: Localization
localizationEn = Localization
	{ locLanguageName = "English"
	, locHome = "itch.io Sanity Checker"
	, locWelcome = "Welcome to itch.io Sanity Checker. Try to search for your game by entering a few keywords, and wait for a report. Beware: it's alpha software, results may be weird."
	, locSearchGameByName = "Search for game by name:"
	, locSearch = "Search"
	, locGoToGameByUrl = "Go to game by url:"
	, locGo = "Go"
	, locNoAffiliation = "This service is unofficial and is not affiliated with itch.io"
	, locGameByAuthor = \game author -> game <> " by " <> author
	, locLink = \url -> "Link: " <> RichText [RichChunkLink url url]
	, locDescription = \desc -> "Description: " <> desc
	, locPlatforms = "Platforms"
	, locHasDemo = "Has demo"
	, locNoDemo = "No demo"
	, locFreeDonationsAllowed = "Free, donations allowed"
	, locMinimumPrice = "Minimum price"
	, locFreePaymentsDisabled = "Free, payments disabled"
	, locOptedInPressSystem = "Opted into itch.io press system"
	, locNotOptedInPressSystem = "Did not opted into itch.io press system"
	, locUploads = "Uploads"
	, locDisplayName = "Display name"
	, locFileName = "File name"
	, locAccessMode = "Mode"
	, locSize = "Size"
	, locTags = "Tags"
	, locReportStatus = "Report status"
	, locSizeInBytes = \size ->
		if size < 2 * 1024 then T.pack (show size) <> " b"
		else if size < 2 * 1024 * 1024 then T.pack (showFFloat (Just 1) (fromIntegral size / 1024 :: Float) "") <> " Kb"
		else if size < 2 * 1024 * 1024 * 1024 then T.pack (showFFloat (Just 1) (fromIntegral size / (1024 * 1024) :: Float) "") <> " Mb"
		else T.pack (showFFloat (Just 1) (fromIntegral size / (1024 * 1024 * 1024) :: Float) "") <> " Gb"
	, locSymlink = "symlink"
	, locNoUserVersion = "not specified"
	, locBuildVersion = \buildVersion buildUserVersion -> "version: " <> RichText [RichChunkCode buildVersion] <> ", user version: " <> RichText [RichChunkCode buildUserVersion]
	, locDoesntUseButler = "doesn't use butler"
	, locInvestigationStarted = "enqueuing..."
	, locInvestigationQueued = \n -> T.pack ('#' : shows n " in queue...")
	, locInvestigationProcessing = "processing..."
	, locInvestigationSucceeded = "report is ready"
	, locInvestigationFailed = "failed"
	, locReinvestigate = "Reprocess"
	, locReport = "Report"
	, locReportNotComplete = \a b -> "Report is not complete! Processed " <> RichText [RichChunkCode $ T.pack $ show a] <> " " <> plural a "upload" "uploads" <> " of " <> RichText [RichChunkCode $ T.pack $ show b] <> "."
	, locRecordSeverity = "Status"
	, locRecordScope = "Scope"
	, locRecordName = "Test"
	, locRecordMessage = "Clarification"
	, locScopeProject = "Project"
	, locScopeUploadGroup = \case
		UploadGroupRelease -> "Release Uploads"
		UploadGroupPreorder -> "Preorder Uploads"
		UploadGroupDemo -> "Demo Uploads"
	, locScopeUpload = \maybeUpload -> RichText [RichChunkCode $ fromMaybe "<?>" maybeUpload]
	, locScopeEntry = \maybeUpload entry -> RichText [RichChunkCode (fromMaybe "<?>" maybeUpload), ": ", RichChunkCode entry]
	, locUnknownGame = "Unknown Game"
	, locGameNotCached = "Requesting information on this game... The page will refresh in a few seconds."
	, locRefresh = "Refresh"
	, locSeverityOk = "OK"
	, locSeverityInfo = "INFO"
	, locSeverityTip = "TIP"
	, locSeverityWarn = "WARN"
	, locSeverityBad = "BAD"
	, locSeverityErr = "ERR"
	, locRecordUploadDisplayNameSet = "Display name is set"
	, locRecordUploadDisplayNameNotSet = "Display name is not set"
	, locMessageUploadDisplayNameNotSet = "Currently your package is displayed as its file name, but you can set display name to whatever you like."
	, locRecordUnknownError = "Unknown error"
	, locRecordDownloadNotStarted = "Download not started"
	, locRecordDownloadSkipped = "Download skipped"
	, locRecordDownloadFailed = "Download failed"
	, locMessageDownloadFailed = \e -> RichText ["Download failed: ", RichChunkCode e]
	, locRecordAVCheckNotStarted = "AV check not started"
	, locRecordAVCheckSkipped = "AV check skipped"
	, locRecordAVCheckOk = "AV check OK"
	, locRecordAVCheckFailed = "AV check failed"
	, locRecordUnpackNotStarted = "Unpacking package skipped"
	, locRecordUnpackFailed = "Unpacking package failed"
	, locRecordDepVersionRequirement = \distroName -> RichText ["Minimum required distro is ", RichChunkCode distroName]
	, locMessageDepVersionRequirement = \depName (reportDepVersionToText -> depVersion) distroName (reportDepVersionToText -> depInDistroVersion) -> RichText
		[ "Required version of ", RichChunkCode depName, " is ", RichChunkCode depVersion
		, " which in turn requires at least ", RichChunkCode distroName
		, " (containing ", RichChunkCode depName, " ", RichChunkCode depInDistroVersion, ")."
		]
	, locRecordUnsafeCharsFileName = "Unsafe characters in file name"
	, locMessageUnsafeCharsFileName = \fileName -> "File name \"" <> RichText [RichChunkCode fileName] <> "\" contains characters which may cause problems with different software/operating systems depending on locale and other factors. General advice is to refrain from using non-ASCII characters, ASCII control characters and any of these: " <> RichText [RichChunkCode "\\/:*?\"<>|"] <> "."
	, locRecordNoBinaries = "No binaries found"
	, locMessageNoBinaries = "No binaries found in this upload. It's totally OK if it's a non-executable package like book, soundtrack or asset pack. Overwise, it's probably some mistake."
	, locRecordBinariesCoverPlatforms = "Binaries exist for all declared platforms"
	, locMessageBinariesCoverPlatforms = "Binaries are found for every platform the upload is tagged with."
	, locRecordUntaggedBinary = \platform -> "No " <> RichText [RichChunkCode platform] <> " tag"
	, locMessageUntaggedBinary = \platform -> "There's " <> RichText [RichChunkText platform] <> " executable in the package, but the package is not tagged as " <> RichText [RichChunkCode platform] <> " one. Probably you forgot to tag the package appropriately, or the other way around, put wrong executable in it."
	, locRecordNoPlatformBinaries = \platform -> "No " <> RichText [RichChunkText platform] <> " binaries"
	, locMessageNoPlatformBinaries = \platform -> "The package is tagged with " <> RichText [RichChunkCode platform] <> " platform, but there's no " <> RichText [RichChunkText platform] <> " executables. If your game doesn't have native executables, dismiss this message. Otherwise you probably forgot to put the executable, or put the wrong one. Or tagged the package with wrong tag."
	, locRecordWindowsBinaryX86Exists = "There's 32-bit Windows binary"
	, locRecordNoWindowsBinaryX86 = "No 32-bit Windows binary"
	, locMessageAboutWindowsBinaryX86 = "32-bit systems are still exist among players, so it's nice to provide 32-bit build (in which case 64-bit is not really necessary, as 32-bit binaries are perfectly playable on 64-bit Windows)."
	, locRecordNoLinuxBinaryX64 = "No 64-bit Linux binary"
	, locRecordNoLinuxBinaryX86 = "No 32-bit Linux binary"
	, locRecordHasLinuxBinaryX64X86 = "Both 64-bit and 32-bit Linux binaries are present"
	, locMessageAboutLinuxBinaryArchs = "It's recommended to provide both 64-bit and 32-bit Linux binaries. Most of the Linux system are 64-bit nowadays. Contrary to other OSes, 32-bit programs usually cannot be launched on 64-bit Linux out-of-the-box, installation of additional (\"multilib\") libraries may be required. It's also impossible to run 64-bit program on 32-bit Linux system, which's still used by some players."
	, locRecordMacOSBinaryX86Exists = "There's 32-bit macOS binary"
	, locRecordNoMacOSBinaryX86 = "No 32-bit macOS binary"
	, locMessageAboutMacOSBinaryX86 = "It's impossible to run 64-bit program on 32-bit macOS system, which's still in use. The recommendation is to provide 32-bit version in addition to 64-bit, or a single universal binary supporting both architectures."
	, locRecordHasDemo = "Free demo available"
	, locRecordNoDemo = "No free demo"
	, locMessageAboutDemo = "You can provide free demo, so players can evaluate your game before buying."
	, locRecordOptedIntoPressSystem = "Opted into itch.io press system"
	, locRecordNotOptedIntoPressSystem = "Not opted into itch.io press system"
	, locMessageAboutPressSystem = "You can opt your account into " <> RichText [RichChunkLink "https://itch.io/press/user-list" "itch.io's press system"] <> ", this will enable special press account users to find your paid games and download it for free."
	, locRecordNoUploads = "There's no uploads"
	}

plural :: Integral n => n -> a -> a -> a
plural (abs -> n) one other = if n == 1 then one else other
