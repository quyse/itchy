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
	, ReportItchToml(..)
	, ReportItchTomlPrereq(..)
	, ReportItchTomlAction(..)
	) where

import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Word
import GHC.Generics(Generic)

data Report = Report
	{ report_error :: !(Maybe T.Text)
	, report_download :: !ReportDownload
	, report_avCheck :: !ReportAVCheck
	, report_unpack :: !ReportUnpack
	, report_itchToml :: !ReportItchToml
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
	| ReportUnpack_succeeded !(HM.HashMap T.Text ReportEntry)
	| ReportUnpack_failed !T.Text
	deriving Generic
instance A.ToJSON ReportUnpack where
	toJSON = A.genericToJSON jsonOptions

data ReportEntry
	= ReportEntry_unknown
		{ reportEntry_mode :: {-# UNPACK #-} !Word32
		}
	| ReportEntry_file
		{ reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_size :: {-# UNPACK #-} !Word64
		}
	| ReportEntry_directory
		{ reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_entries :: !(HM.HashMap T.Text ReportEntry)
		}
	| ReportEntry_symlink
		{ reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_link :: !T.Text
		}
	deriving Generic
instance A.ToJSON ReportEntry where
	toJSON = A.genericToJSON jsonOptions

data ReportItchToml
	= ReportItchToml_notStarted
	| ReportItchToml_missing
	| ReportItchToml_failed !T.Text
	| ReportItchToml_malformed
	| ReportItchToml_ok
		{ reportItchToml_prereqs :: ![ReportItchTomlPrereq]
		, reportItchToml_actions :: ![ReportItchTomlAction]
		}
	| ReportItchToml_wrong
		{ reportItchToml_error :: !T.Text
		}
	deriving Generic
instance A.ToJSON ReportItchToml where
	toJSON = A.genericToJSON jsonOptions

data ReportItchTomlPrereq = ReportItchTomlPrereq
	{ reportItchTomlPrereq_name :: !T.Text
	} deriving Generic
instance A.ToJSON ReportItchTomlPrereq where
	toJSON = A.genericToJSON jsonOptions

data ReportItchTomlAction = ReportItchTomlAction
	{ reportItchTomlAction_name :: !T.Text
	, reportItchTomlAction_path :: !T.Text
	, reportItchTomlAction_icon :: !T.Text
	, reportItchTomlAction_scope :: !T.Text
	, reportItchTomlAction_args :: [T.Text]
	, reportItchTomlAction_locales :: !(HM.HashMap T.Text ReportItchTomlAction)
	} deriving Generic
instance A.ToJSON ReportItchTomlAction where
	toJSON = A.genericToJSON jsonOptions

jsonOptions :: A.Options
jsonOptions = A.defaultOptions
	{ A.fieldLabelModifier = tail . dropWhile (/= '_')
	, A.constructorTagModifier = tail . dropWhile (/= '_')
	, A.omitNothingFields = True
	, A.sumEncoding = A.ObjectWithSingleField
	}
