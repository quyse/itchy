{-|
Module: Itchy.Localization.En
Description: English localization.
-}

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Itchy.Localization.En
	( localizationEn
	) where

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Numeric

import Itchy.Localization
import Itchy.Localization.RichText
import Itchy.Report.Record

localizationEn :: Localization
localizationEn = Localization
	{ locLanguageName = "English"
	, locDashboard = "Dashboard"
	, locGames = "Games"
	, locReports = "Reports"
	, locGameByAuthor = \game author -> game <> " by " <> author
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
	, locSize = "Size"
	, locTags = "Tags"
	, locSizeInBytes = \size ->
		if size < 2 * 1024 then T.pack (show size) <> " b"
		else if size < 2 * 1024 * 1024 then T.pack (showFFloat (Just 1) (fromIntegral size / 1024 :: Float) "") <> " Kb"
		else if size < 2 * 1024 * 1024 * 1024 then T.pack (showFFloat (Just 1) (fromIntegral size / (1024 * 1024) :: Float) "") <> " Mb"
		else T.pack (showFFloat (Just 1) (fromIntegral size / (1024 * 1024 * 1024) :: Float) "") <> " Gb"
	, locReport = "Report"
	, locRecordSeverity = "Severity"
	, locRecordScope = "Scope"
	, locRecordName = "Test"
	, locScopeUploadGroup = \case
		UploadGroupRelease -> "Release Uploads"
		UploadGroupPreorder -> "Preorder Uploads"
		UploadGroupDemo -> "Demo Uploads"
	, locScopeUpload = \maybeUpload -> RichText [RichChunkCode $ fromMaybe "<?>" maybeUpload]
	, locScopeEntry = \maybeUpload entry -> RichText [RichChunkCode (fromMaybe "<?>" maybeUpload), RichChunkText ": ", RichChunkCode entry]
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
	, locRecordAVCheckNotStarted = "AV check not started"
	, locRecordAVCheckSkipped = "AV check skipped"
	, locRecordAVCheckOk = "AV check OK"
	, locRecordAVCheckFailed = "AV check failed"
	, locRecordUnpackNotStarted = "Unpacking package skipped"
	, locRecordUnpackFailed = "Unpacking package failed"
	, locRecordNoBinaries = "No binaries found"
	, locMessageNoBinaries = "No binaries found in this upload. It's totally OK if it's a non-executable package like book, soundtrack or asset pack."
	, locRecordBinariesCoverPlatforms = "Binaries exist for all declared platforms"
	, locMessageBinariesCoverPlatforms = "Binaries have been found for every platform the upload is tagged with."
	, locRecordBinariesPlatformsMismatch = "Binaries platforms mismatch"
	, locMessageBinariesPlatformsMismatch = "Binaries platforms mismatch"
	, locRecordWindowsBinaryX86Exists = "There's 32-bit Windows binary"
	, locRecordNoWindowsBinaryX86 = "No 32-bit Windows binary"
	, locMessageAboutWindowsBinaryX86 = "Your Windows build can be played on 64-bit system only. 32-bit systems are still exist among players, so it would be nice to provide 32-bit build (in which case 64-bit is not really necessary, as 32-bit binaries are perfectly playable on 64-bit Windows)."
	, locRecordNoLinuxBinaryX64 = "No 64-bit Linux binary"
	, locMessageNoLinuxBinaryX64 = "Most of the Linux system are 64-bit nowadays. Contrary to other OSes, 32-bit programs usually cannot be launched on 64-bit Linux out-of-the-box, installation of additional (\"multilib\") libraries may be required. The strong recommendation is to provide 64-bit version among with 32-bit, to not create troubles to the actual majority of users."
	, locRecordNoLinuxBinaryX86 = "No 32-bit Linux binary"
	, locMessageNoLinuxBinaryX86 = "It's impossible to run 64-bit program on 32-bit Linux system, which's still used by some players. It's advised to provide 32-bit version for them in addition to 64-bit."
	, locRecordMacOSBinaryX86Exists = "There's 32-bit macOS binary"
	, locRecordNoMacOSBinaryX86 = "No 32-bit macOS binary"
	, locMessageAboutMacOSBinaryX86 = "It's impossible to run 64-bit program on 32-bit macOS system, which's still in use. The recommendation is to provide 32-bit version in addition to 64-bit, or a single universal binary supporting both architectures."
	, locRecordNoUploads = "There're no uploads"
	}
