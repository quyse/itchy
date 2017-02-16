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
	, ReportParse(..)
	, ItchToml(..)
	, ItchTomlPrereq(..)
	, ItchTomlAction(..)
	, ReportBinaryPe(..)
	, ReportBinaryElf(..)
	, ReportBinaryMachO(..)
	, ReportMachOSubBinary(..)
	, ReportArch(..)
	, ReportDep(..)
	) where

import qualified Data.Aeson.Types as A
import qualified Data.Map.Strict as M
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
instance A.FromJSON Report where
	parseJSON = A.genericParseJSON jsonOptions

data ReportDownload
	= ReportDownload_notStarted
	| ReportDownload_skipped
	| ReportDownload_succeeded
	| ReportDownload_failed !T.Text
	deriving Generic
instance A.ToJSON ReportDownload where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportDownload where
	parseJSON = A.genericParseJSON jsonOptions

data ReportAVCheck
	= ReportAVCheck_notStarted
	| ReportAVCheck_ok
	| ReportAVCheck_failed !T.Text
	deriving Generic
instance A.ToJSON ReportAVCheck where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportAVCheck where
	parseJSON = A.genericParseJSON jsonOptions

data ReportUnpack
	= ReportUnpack_notStarted
	| ReportUnpack_succeeded !(M.Map T.Text ReportEntry)
	| ReportUnpack_failed !T.Text
	deriving Generic
instance A.ToJSON ReportUnpack where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportUnpack where
	parseJSON = A.genericParseJSON jsonOptions

data ReportEntry
	= ReportEntry_unknown
		{ reportEntry_mode :: {-# UNPACK #-} !Word32
		}
	| ReportEntry_file
		{ reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_size :: {-# UNPACK #-} !Word64
		, reportEntry_mime :: !T.Text
		, reportEntry_parses :: ![ReportParse]
		}
	| ReportEntry_directory
		{ reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_entries :: !(M.Map T.Text ReportEntry)
		}
	| ReportEntry_symlink
		{ reportEntry_mode :: {-# UNPACK #-} !Word32
		, reportEntry_link :: !T.Text
		}
	deriving Generic
instance A.ToJSON ReportEntry where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportEntry where
	parseJSON = A.genericParseJSON jsonOptions

data ReportParse
	= ReportParse_itchToml !(Either T.Text ItchToml)
	| ReportParse_binaryPe !ReportBinaryPe
	| ReportParse_binaryElf !ReportBinaryElf
	| ReportParse_binaryMachO !ReportBinaryMachO
	deriving Generic
instance A.ToJSON ReportParse where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportParse where
	parseJSON = A.genericParseJSON jsonOptions

data ItchToml = ItchToml
	{ itchToml_prereqs :: !(Maybe [ItchTomlPrereq])
	, itchToml_actions :: [ItchTomlAction]
	} deriving Generic
instance A.ToJSON ItchToml where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ItchToml where
	parseJSON = A.genericParseJSON jsonOptions

data ItchTomlPrereq = ItchTomlPrereq
	{ itchTomlPrereq_name :: !T.Text
	} deriving Generic
instance A.ToJSON ItchTomlPrereq where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ItchTomlPrereq where
	parseJSON = A.genericParseJSON jsonOptions

data ItchTomlAction = ItchTomlAction
	{ itchTomlAction_name :: !T.Text
	, itchTomlAction_path :: !T.Text
	, itchTomlAction_icon :: !(Maybe T.Text)
	, itchTomlAction_scope :: !(Maybe T.Text)
	, itchTomlAction_args :: !(Maybe [T.Text])
	} deriving Generic
instance A.ToJSON ItchTomlAction where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ItchTomlAction where
	parseJSON = A.genericParseJSON jsonOptions

data ReportBinaryPe = ReportBinaryPe
	{ reportBinaryPe_arch :: !ReportArch
	, reportBinaryPe_deps :: [ReportDep]
	} deriving Generic
instance A.ToJSON ReportBinaryPe where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportBinaryPe where
	parseJSON = A.genericParseJSON jsonOptions

data ReportBinaryElf = ReportBinaryElf
	{ reportBinaryElf_arch :: !ReportArch
	, reportBinaryElf_deps :: [ReportDep]
	} deriving Generic
instance A.ToJSON ReportBinaryElf where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportBinaryElf where
	parseJSON = A.genericParseJSON jsonOptions

data ReportBinaryMachO = ReportBinaryMachO
	{ reportBinary_binaries :: [ReportMachOSubBinary]
	} deriving Generic
instance A.ToJSON ReportBinaryMachO where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportBinaryMachO where
	parseJSON = A.genericParseJSON jsonOptions

data ReportMachOSubBinary = ReportMachOSubBinary
	{ reportMachoSubBinary_arch :: !ReportArch
	, reportMachoSubBinary_deps :: [ReportDep]
	} deriving Generic
instance A.ToJSON ReportMachOSubBinary where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportMachOSubBinary where
	parseJSON = A.genericParseJSON jsonOptions

data ReportArch
	= ReportArch_unknown
	| ReportArch_x86
	| ReportArch_x64
	deriving Generic
instance A.ToJSON ReportArch where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportArch where
	parseJSON = A.genericParseJSON jsonOptions

data ReportDep = ReportDep
	{ reportDep_name :: !T.Text
	, reportDep_version :: !T.Text
	} deriving Generic
instance A.ToJSON ReportDep where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportDep where
	parseJSON = A.genericParseJSON jsonOptions

jsonOptions :: A.Options
jsonOptions = A.defaultOptions
	{ A.fieldLabelModifier = tail . dropWhile (/= '_')
	, A.constructorTagModifier = tail . dropWhile (/= '_')
	, A.omitNothingFields = True
	, A.sumEncoding = A.ObjectWithSingleField
	}
