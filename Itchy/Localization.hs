{-|
Module: Itchy.Localization
Description: Localization
-}

module Itchy.Localization
	( Localization(..)
	) where

import Data.Monoid()
import qualified Data.Text as T

import Itchy.Localization.RichText
import Itchy.Report
import Itchy.Report.Record

data Localization = Localization
	{ locLanguageName :: !T.Text
	, locHome :: !T.Text
	, locSearch :: !T.Text
	, locGameByAuthor :: !(T.Text -> T.Text -> T.Text)
	, locDescription :: !(T.Text -> T.Text)
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
	, locNoUserVersion :: !T.Text
	, locBuildVersion :: !(T.Text -> T.Text -> RichText)
	, locDoesntUseButler :: !T.Text
	, locInvestigationStarted :: !T.Text
	, locInvestigationQueued :: !T.Text
	, locInvestigationProcessing :: !T.Text
	, locInvestigationSucceeded :: !T.Text
	, locInvestigationFailed :: !T.Text
	, locReinvestigate :: !T.Text
	, locReport :: !T.Text
	, locReportNotComplete :: !(Int -> Int -> RichText)
	, locRecordSeverity :: !T.Text
	, locRecordScope :: !T.Text
	, locRecordName :: !T.Text
	, locScopeProject :: !RichText
	, locScopeUploadGroup :: !(UploadGroup -> RichText)
	, locScopeUpload :: !(Maybe T.Text -> RichText)
	, locScopeEntry :: !(Maybe T.Text -> T.Text -> RichText)
	, locUnknownGame :: !T.Text
	, locGameNotCached :: !T.Text
	, locRefresh :: !T.Text
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
	, locRecordDepVersionRequirement :: !(T.Text -> RichText)
	, locMessageDepVersionRequirement :: !(T.Text -> ReportDepVersion -> T.Text -> ReportDepVersion -> RichText)
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
	, locRecordNoLinuxBinaryX86 :: !RichText
	, locRecordHasLinuxBinaryX64X86 :: !RichText
	, locMessageAboutLinuxBinaryArchs :: !RichText
	, locRecordMacOSBinaryX86Exists :: !RichText
	, locRecordNoMacOSBinaryX86 :: !RichText
	, locMessageAboutMacOSBinaryX86 :: !RichText
	, locRecordHasDemo :: !RichText
	, locRecordNoDemo :: !RichText
	, locMessageAboutDemo :: !RichText
	, locRecordOptedIntoPressSystem :: !RichText
	, locRecordNotOptedIntoPressSystem :: !RichText
	, locMessageAboutPressSystem :: !RichText
	, locRecordNoUploads :: !RichText
	}
