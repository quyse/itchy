{-|
Module: Itchy.Report.Record
Description: Report records.
-}

module Itchy.Report.Record
	( Record(..)
	, Severity(..)
	, Scope(..)
	, UploadGroup(..)
	) where

import qualified Data.Text as T

import Itchy.Itch
import Itchy.Localization.RichText

data Record = Record
	{ recordScope :: !Scope
	, recordSeverity :: !Severity
	, recordName :: !RichText
	, recordMessage :: !RichText
	}

data Scope
	= ProjectScope
	| UploadGroupScope !UploadGroup
	| UploadScope {-# UNPACK #-} !ItchUploadId
	| EntryScope {-# UNPACK #-} !ItchUploadId [T.Text]
	deriving (Eq, Ord)

data UploadGroup
	= UploadGroupRelease
	| UploadGroupPreorder
	| UploadGroupDemo
	deriving (Eq, Ord)

data Severity
	= SeverityErr
	| SeverityBad
	| SeverityWarn
	| SeverityTip
	| SeverityInfo
	| SeverityOk
	deriving (Eq, Ord)
