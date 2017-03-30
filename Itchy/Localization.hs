{-|
Module: Itchy.Localization
Description: Localization
-}

module Itchy.Localization
	( Localization(..)
	) where

import Data.Monoid()
import qualified Data.Text as T
import Itchy.Report.Record

import Itchy.Localization.RichText

data Localization = Localization
	{ locLanguageName :: !T.Text
	, locDashboard :: !T.Text
	, locGames :: !T.Text
	, locReports :: !T.Text
	, locGameByAuthor :: !(T.Text -> T.Text -> T.Text)
	, locPlatforms :: !T.Text
	, locHasDemo :: !T.Text
	, locNoDemo :: !T.Text
	, locFreeDonationsAllowed :: !T.Text
	, locMinimumPrice :: !T.Text
	, locFreePaymentsDisabled :: !T.Text
	, locOptedInPressSystem :: !T.Text
	, locNotOptedInPressSystem :: !T.Text
	, locUploads :: !T.Text
	, locDisplayName :: !T.Text
	, locFileName :: !T.Text
	, locSize :: !T.Text
	, locTags :: !T.Text
	, locSizeInBytes :: !(Integer -> T.Text)
	, locReport :: !T.Text
	, locRecordSeverity :: !T.Text
	, locRecordScope :: !T.Text
	, locRecordName :: !T.Text
	, locScopeUploadGroup :: !(UploadGroup -> RichText)
	, locScopeUpload :: !(Maybe T.Text -> RichText)
	, locScopeEntry :: !(Maybe T.Text -> T.Text -> RichText)
	, locSeverityOk :: !T.Text
	, locSeverityInfo :: !T.Text
	, locSeverityTip :: !T.Text
	, locSeverityWarn :: !T.Text
	, locSeverityBad :: !T.Text
	, locSeverityErr :: !T.Text
	, locRecordUploadDisplayNameSet :: !RichText
	, locRecordUploadDisplayNameNotSet :: !RichText
	, locMessageUploadDisplayNameNotSet :: !RichText
	, locRecordUnknownError :: !RichText
	, locRecordAVCheckNotStarted :: !RichText
	, locRecordAVCheckSkipped :: !RichText
	, locRecordAVCheckOk :: !RichText
	, locRecordAVCheckFailed :: !RichText
	, locRecordUnpackNotStarted :: !RichText
	, locRecordUnpackFailed :: !RichText
	, locRecordNoBinaries :: !RichText
	, locMessageNoBinaries :: !RichText
	, locRecordBinariesCoverPlatforms :: !RichText
	, locMessageBinariesCoverPlatforms :: !RichText
	, locRecordBinariesPlatformsMismatch :: !RichText
	, locMessageBinariesPlatformsMismatch :: !RichText
	, locRecordWindowsBinaryX86Exists :: !RichText
	, locRecordNoWindowsBinaryX86 :: !RichText
	, locMessageAboutWindowsBinaryX86 :: !RichText
	, locRecordNoLinuxBinaryX64 :: !RichText
	, locMessageNoLinuxBinaryX64 :: !RichText
	, locRecordNoLinuxBinaryX86 :: !RichText
	, locMessageNoLinuxBinaryX86 :: !RichText
	, locRecordMacOSBinaryX86Exists :: !RichText
	, locRecordNoMacOSBinaryX86 :: !RichText
	, locMessageAboutMacOSBinaryX86 :: !RichText
	, locRecordNoUploads :: !RichText
	}
