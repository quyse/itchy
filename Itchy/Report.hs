{-|
Module: Itchy.Report
Description: Report structures.
-}

{-# LANGUAGE DeriveGeneric #-}

module Itchy.Report
	( Report(..)
	, ReportDownload(..)
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
	, report_unpack :: !ReportUnpack
	} deriving Generic
instance A.FromJSON Report where
	parseJSON = A.genericParseJSON jsonOptions
instance A.ToJSON Report where
	toJSON = A.genericToJSON jsonOptions

data ReportDownload
	= ReportDownload_notStarted
	| ReportDownload_skipped
	| ReportDownload_succeeded
	| ReportDownload_failed !T.Text
	deriving Generic
instance A.FromJSON ReportDownload where
	parseJSON = A.genericParseJSON jsonOptions
instance A.ToJSON ReportDownload where
	toJSON = A.genericToJSON jsonOptions

data ReportUnpack
	= ReportUnpack_notStarted
	| ReportUnpack_skipped
	| ReportUnpack_succeeded [ReportEntry]
	| ReportUnpack_failed !T.Text
	deriving Generic
instance A.FromJSON ReportUnpack where
	parseJSON = A.genericParseJSON jsonOptions
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
instance A.FromJSON ReportEntry where
	parseJSON = A.genericParseJSON jsonOptions
instance A.ToJSON ReportEntry where
	toJSON = A.genericToJSON jsonOptions

jsonOptions :: A.Options
jsonOptions = A.defaultOptions
	{ A.fieldLabelModifier = tail . dropWhile (/= '_')
	, A.constructorTagModifier = tail . dropWhile (/= '_')
	, A.omitNothingFields = True
	, A.sumEncoding = A.ObjectWithSingleField
	}
