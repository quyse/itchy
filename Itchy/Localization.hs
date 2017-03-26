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

newtype RichText = RichText [RichChunk] deriving Monoid

instance IsString RichText where
	fromString s = RichText [fromString s]

data RichChunk
	= RichChunkText !T.Text
	| RichChunkLink !T.Text !T.Text
	| RichChunkCode !T.Text

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
	, locReportWithNumber :: !(Int -> T.Text)
	, locGeneralError :: !(T.Text -> T.Text)
	, locDownloadFailed :: !(T.Text -> T.Text)
	, locAVCheckOk :: !T.Text
	, locAVCheckFailed :: !(T.Text -> T.Text)
	, locUnpackFailed :: !(T.Text -> T.Text)
	}
