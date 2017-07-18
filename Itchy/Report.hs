{-|
Module: Itchy.Report
Description: Report structures.
-}

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

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
	, ReportDepVersion(..)
	, reportDepVersionToText
	) where

import qualified Data.Aeson.Types as A
import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.Serialize as S
import Data.Serialize.Text()
import qualified Data.Text as T
import Data.Word
import GHC.Generics(Generic)

-- | Report contains information on a single upload.
data Report = Report
	{ report_error :: !(Maybe T.Text)
	, report_download :: !ReportDownload
	, report_avCheck :: !ReportAVCheck
	, report_unpack :: !ReportUnpack
	} deriving Generic
instance S.Serialize Report
instance A.ToJSON Report where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON Report where
	parseJSON = A.genericParseJSON jsonOptions

-- | Information about download errors.
data ReportDownload
	= ReportDownload_notStarted
	| ReportDownload_skipped
	| ReportDownload_succeeded
	| ReportDownload_failed !T.Text
	deriving Generic
instance S.Serialize ReportDownload
instance A.ToJSON ReportDownload where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportDownload where
	parseJSON = A.genericParseJSON jsonOptions

-- | Information about anti-virus check.
data ReportAVCheck
	= ReportAVCheck_notStarted
	| ReportAVCheck_skipped
	| ReportAVCheck_ok
	| ReportAVCheck_failed !T.Text
	deriving Generic
instance S.Serialize ReportAVCheck
instance A.ToJSON ReportAVCheck where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportAVCheck where
	parseJSON = A.genericParseJSON jsonOptions

-- | Information about unpacking errors.
data ReportUnpack
	= ReportUnpack_notStarted
	| ReportUnpack_succeeded !(M.Map T.Text ReportEntry)
	| ReportUnpack_failed !T.Text
	deriving Generic
instance S.Serialize ReportUnpack
instance A.ToJSON ReportUnpack where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportUnpack where
	parseJSON = A.genericParseJSON jsonOptions

-- | Information about a single file/directory.
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
instance S.Serialize ReportEntry
instance A.ToJSON ReportEntry where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportEntry where
	parseJSON = A.genericParseJSON jsonOptions

-- | Parsed information about file.
data ReportParse
	= ReportParse_itchToml !(Either T.Text ItchToml)
	| ReportParse_binaryPe !ReportBinaryPe
	| ReportParse_binaryElf !ReportBinaryElf
	| ReportParse_binaryMachO !ReportBinaryMachO
	deriving Generic
instance S.Serialize ReportParse
instance A.ToJSON ReportParse where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportParse where
	parseJSON = A.genericParseJSON jsonOptions

-- | Parsed information about @.itch.toml@ file.
data ItchToml = ItchToml
	{ itchToml_prereqs :: !(Maybe [ItchTomlPrereq])
	, itchToml_actions :: [ItchTomlAction]
	} deriving Generic
instance S.Serialize ItchToml
instance A.ToJSON ItchToml where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ItchToml where
	parseJSON = A.genericParseJSON jsonOptions

-- | Prerequisite in @.itch.toml@.
data ItchTomlPrereq = ItchTomlPrereq
	{ itchTomlPrereq_name :: !T.Text
	} deriving Generic
instance S.Serialize ItchTomlPrereq
instance A.ToJSON ItchTomlPrereq where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ItchTomlPrereq where
	parseJSON = A.genericParseJSON jsonOptions

-- | Action in @.itch.toml@.
data ItchTomlAction = ItchTomlAction
	{ itchTomlAction_name :: !T.Text
	, itchTomlAction_path :: !T.Text
	, itchTomlAction_icon :: !(Maybe T.Text)
	, itchTomlAction_scope :: !(Maybe T.Text)
	, itchTomlAction_args :: !(Maybe [T.Text])
	} deriving Generic
instance S.Serialize ItchTomlAction
instance A.ToJSON ItchTomlAction where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ItchTomlAction where
	parseJSON = A.genericParseJSON jsonOptions

-- | Windows PE executable/library.
data ReportBinaryPe = ReportBinaryPe
	{ reportBinaryPe_arch :: !ReportArch
	, reportBinaryPe_isLibrary :: !Bool
	, reportBinaryPe_isCLR :: !Bool
	, reportBinaryPe_deps :: [ReportDep]
	} deriving Generic
instance S.Serialize ReportBinaryPe
instance A.ToJSON ReportBinaryPe where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportBinaryPe where
	parseJSON = A.genericParseJSON jsonOptions

-- | Linux ELF binary.
data ReportBinaryElf = ReportBinaryElf
	{ reportBinaryElf_arch :: !ReportArch
	, reportBinaryElf_isLibrary :: !Bool
	, reportBinaryElf_deps :: [ReportDep]
	} deriving Generic
instance S.Serialize ReportBinaryElf
instance A.ToJSON ReportBinaryElf where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportBinaryElf where
	parseJSON = A.genericParseJSON jsonOptions

-- | macOS Mach-O binary.
data ReportBinaryMachO = ReportBinaryMachO
	{ reportBinaryMachO_binaries :: [ReportMachOSubBinary]
	, reportBinaryMachO_isLibrary :: !Bool
	} deriving Generic
instance S.Serialize ReportBinaryMachO
instance A.ToJSON ReportBinaryMachO where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportBinaryMachO where
	parseJSON = A.genericParseJSON jsonOptions

-- | macOS Mach-O sub-binary.
data ReportMachOSubBinary = ReportMachOSubBinary
	{ reportMachoSubBinary_arch :: !ReportArch
	, reportMachoSubBinary_deps :: [ReportDep]
	} deriving Generic
instance S.Serialize ReportMachOSubBinary
instance A.ToJSON ReportMachOSubBinary where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportMachOSubBinary where
	parseJSON = A.genericParseJSON jsonOptions

-- | Binary architecture.
data ReportArch
	= ReportArch_unknown
	| ReportArch_x86
	| ReportArch_x64
	deriving (Eq, Ord, Generic)
instance S.Serialize ReportArch
instance Hashable ReportArch
instance A.ToJSON ReportArch where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportArch where
	parseJSON = A.genericParseJSON jsonOptions

-- | Binary dependency.
data ReportDep = ReportDep
	{ reportDep_name :: !T.Text
	, reportDep_version :: !ReportDepVersion
	} deriving Generic
instance S.Serialize ReportDep
instance A.ToJSON ReportDep where
	toJSON = A.genericToJSON jsonOptions
instance A.FromJSON ReportDep where
	parseJSON = A.genericParseJSON jsonOptions

-- | Parsed version of binary dependency.
newtype ReportDepVersion = ReportDepVersion [Integer]
	deriving (Generic, Eq, Ord, S.Serialize, A.ToJSON, A.FromJSON)

-- | Convert version of binary dependency to text.
reportDepVersionToText :: ReportDepVersion -> T.Text
reportDepVersionToText (ReportDepVersion numbers) = T.intercalate "." $ map (T.pack . show) numbers

jsonOptions :: A.Options
jsonOptions = A.defaultOptions
	{ A.fieldLabelModifier = tail . dropWhile (/= '_')
	, A.constructorTagModifier = tail . dropWhile (/= '_')
	, A.omitNothingFields = True
	, A.sumEncoding = A.ObjectWithSingleField
	}
