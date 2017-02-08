{-|
Module: Itchy.Report
Description: Report structures.
-}

{-# LANGUAGE DeriveGeneric #-}

module Itchy.Report
	( Report(..)
	, ReportDownload(..)
	, ReportUnpack(..)
	) where

import qualified Data.Aeson.Types as A
import qualified Data.Text as T
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
	| ReportUnpack_succeeded
	| ReportUnpack_failed !T.Text
	deriving Generic
instance A.FromJSON ReportUnpack where
	parseJSON = A.genericParseJSON jsonOptions
instance A.ToJSON ReportUnpack where
	toJSON = A.genericToJSON jsonOptions

jsonOptions :: A.Options
jsonOptions = A.defaultOptions
	{ A.fieldLabelModifier = tail . dropWhile (/= '_')
	, A.constructorTagModifier = tail . dropWhile (/= '_')
	, A.omitNothingFields = True
	, A.sumEncoding = A.ObjectWithSingleField
	}
