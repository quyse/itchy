{-|
Module: Itchy.Localization.RichText
Description: RichText
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings #-}

module Itchy.Localization.RichText
	( RichText(..)
	, RichChunk(..)
	) where

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
		RichChunkLink l t -> H.a H.! A.href (H.toValue l) H.! A.target "_blank" $ H.toHtml t
		RichChunkCode t -> H.code $ H.toHtml t
