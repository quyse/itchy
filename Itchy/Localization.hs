{-|
Module: Itchy.Localization
Description: Localization
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

module Itchy.Localization
	( RichText(..)
	, RichChunk(..)
	, Localization(..)
	) where

import Data.Monoid()
import Data.String
import qualified Data.Text as T
import qualified Text.Blaze as TB
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

newtype RichText = RichText [RichChunk] deriving (Eq, Ord, Monoid)

instance IsString RichText where
	fromString s = RichText [fromString s]

instance TB.ToMarkup RichText where
	toMarkup (RichText chunks) = mconcat $ map TB.toMarkup chunks

data RichChunk
	= RichChunkText !T.Text
	| RichChunkLink !T.Text !T.Text
	| RichChunkCode !T.Text
	deriving (Eq, Ord)

instance IsString RichChunk where
	fromString = RichChunkText . T.pack

instance TB.ToMarkup RichChunk where
	toMarkup = \case
		RichChunkText t -> H.toHtml t
		RichChunkLink l t -> H.a H.! A.href (H.toValue l) $ H.toHtml t
		RichChunkCode t -> H.code $ H.toHtml t

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
	, locRecordMessage :: !T.Text
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
	, locRecordNoUploads :: !RichText
	}
