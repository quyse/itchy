{-|
Module: Itchy.Localization.En
Description: English localization.
-}

{-# LANGUAGE OverloadedStrings #-}

module Itchy.Localization.En
	( localizationEn
	) where

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Numeric

import Itchy.Localization

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
	, locRecordMessage = "Description"
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
	, locRecordNoUploads = "There're no uploads"
	}
