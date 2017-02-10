{-|
Module: Itchy.Report
Description: Report structures.
-}

{-# LANGUAGE DeriveGeneric #-}

module Itchy.Report
	( Report(..)
	, ReportDownload(..)
	, ReportAVCheck(..)
	, ReportUnpack(..)
	, ReportEntry(..)
	) where

import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Data.Word
import GHC.Generics(Generic)

data Report = Report
	{ report_error :: !(Maybe T.Text)
	, report_download :: !ReportDownload
	, report_avCheck :: !ReportAVCheck
	, report_unpack :: !ReportUnpack
	} deriving Generic
instance A.ToJSON Report where
	toJSON = A.genericToJSON jsonOptions

data ReportDownload
	= ReportDownload_notStarted
	| ReportDownload_skipped
	| ReportDownload_succeeded
	| ReportDownload_failed !T.Text
	deriving Generic
instance A.ToJSON ReportDownload where
	toJSON = A.genericToJSON jsonOptions

data ReportAVCheck
	= ReportAVCheck_notStarted
	| ReportAVCheck_ok
	| ReportAVCheck_failed !T.Text
	deriving Generic
instance A.ToJSON ReportAVCheck where
	toJSON = A.genericToJSON jsonOptions

data ReportUnpack
	= ReportUnpack_notStarted
	| ReportUnpack_skipped
	| ReportUnpack_succeeded [ReportEntry]
	| ReportUnpack_failed !T.Text
	deriving Generic
instance A.ToJSON ReportUnpack where
	toJSON = A.genericToJSON jsonOptions

data ReportEntry
	= ReportEntry_unknown
		{ reportEntry_name :: !T.Text
		, reportEntry_mode :: {-# UNPACK #-} !Word32
		}
	| ReportEntry_file
		{ reportEntry_name :: !T.Text
		, reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_size :: {-# UNPACK #-} !Word64
		}
	| ReportEntry_directory
		{ reportEntry_name :: !T.Text
		, reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_entries :: [ReportEntry]
		}
	| ReportEntry_symlink
		{ reportEntry_name :: !T.Text
		, reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_link :: !T.Text
		}
	deriving Generic
instance A.ToJSON ReportEntry where
	toJSON = A.genericToJSON jsonOptions

jsonOptions :: A.Options
jsonOptions = A.defaultOptions
	{ A.fieldLabelModifier = tail . dropWhile (/= '_')
	, A.constructorTagModifier = tail . dropWhile (/= '_')
	, A.omitNothingFields = True
	, A.sumEncoding = A.ObjectWithSingleField
	}
