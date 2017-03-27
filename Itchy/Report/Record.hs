{-|
Module: Itchy.Report.Record
Description: Report records.
-}

module Itchy.Report.Record
	( Record(..)
	, Severity(..)
	, Scope(..)
	) where

import qualified Data.Text as T

import Itchy.Itch
import Itchy.Localization

data Record = Record
	{ recordScope :: !Scope
	, recordSeverity :: !Severity
	, recordName :: !RichText
	, recordMessage :: !RichText
	}

data Scope
	= GameScope
	| UploadScope {-# UNPACK #-} !ItchUploadId
	| EntryScope {-# UNPACK #-} !ItchUploadId [T.Text]
	deriving (Eq, Ord)

data Severity
	= SeverityErr
	| SeverityBad
	| SeverityWarn
	| SeverityTip
	| SeverityInfo
	| SeverityOk
	deriving (Eq, Ord)
