{-|
Module: Itchy.Report.Analysis
Description: Report analysis structures.
-}

{-# LANGUAGE DeriveGeneric, LambdaCase, PatternSynonyms, ViewPatterns #-}

module Itchy.Report.Analysis
	( AnalysisGame(..)
	, AnalysisUpload(..)
	, AnalysisUploadGroup(..)
	, analyseGame
	) where

import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics(Generic)

import Itchy.Itch
import Itchy.Localization
import Itchy.Report
import Itchy.Report.Record

data AnalysisGame = AnalysisGame
	{ analysisGame_uploads :: [AnalysisUpload]
	, analysisGame_release :: !AnalysisUploadGroup
	, analysisGame_preorder :: !AnalysisUploadGroup
	, analysisGame_demo :: !AnalysisUploadGroup
	, analysisGame_records :: [Record]
	}

data AnalysisUpload = AnalysisUpload
	{ analysisUpload_itchUpload :: !ItchUpload
	, analysisUpload_report :: !Report
	, analysisUpload_tagsPlatforms :: !(HS.HashSet Platform)
	, analysisUpload_binariesPlatforms :: !(HS.HashSet Platform)
	, analysisUpload_records :: [Record]
	}

data AnalysisUploadGroup = AnalysisUploadGroup
	{ analysisUploadGroup_uploads :: [AnalysisUpload]
	, analysisUploadGroup_platformsArchs :: !(HS.HashSet (Int, ReportArch))
	}

data Platform
	= PlatformWindows
	| PlatformLinux
	| PlatformMacOS
	| PlatformAndroid
	deriving (Eq, Enum, Generic)
instance Hashable Platform where

analyseUpload :: Localization -> ItchUpload -> Report -> AnalysisUpload
analyseUpload loc itchUpload@ItchUpload
	{ itchUpload_id = UploadScope -> scope
	, itchUpload_display_name = maybeDisplayName
	, itchUpload_p_windows = tagWindows
	, itchUpload_p_linux = tagLinux
	, itchUpload_p_osx = tagMacOS
	, itchUpload_p_android = tagAndroid
	} report = AnalysisUpload
	{ analysisUpload_itchUpload = itchUpload
	, analysisUpload_report = report
	, analysisUpload_tagsPlatforms = tagsPlatforms
	, analysisUpload_binariesPlatforms = binariesPlatforms
	, analysisUpload_records
		= displayNameCheckRecord
		: avCheckRecord
		: generalErrorCheckRecords
		++ unpackCheckRecords
	} where
	-- check for having display name
	displayNameCheckRecord = case maybeDisplayName of
		Just (T.null -> False) -> Record
			{ recordScope = scope
			, recordSeverity = SeverityOk
			, recordName = locRecordUploadDisplayNameSet loc
			, recordMessage = mempty
			}
		_ -> Record
			{ recordScope = scope
			, recordSeverity = SeverityTip
			, recordName = locRecordUploadDisplayNameNotSet loc
			, recordMessage = locMessageUploadDisplayNameNotSet loc
			}
	-- general error check
	generalErrorCheckRecords = case report_error report of
		Just e -> [Record
			{ recordScope = scope
			, recordSeverity = SeverityErr
			, recordName = locRecordUnknownError loc
			, recordMessage = RichText [RichChunkCode e]
			}]
		Nothing -> []
	-- AV check
	avCheckRecord = case report_avCheck report of
		ReportAVCheck_notStarted -> Record
			{ recordScope = scope
			, recordSeverity = SeverityInfo
			, recordName = locRecordAVCheckNotStarted loc
			, recordMessage = mempty
			}
		ReportAVCheck_skipped -> Record
			{ recordScope = scope
			, recordSeverity = SeverityInfo
			, recordName = locRecordAVCheckSkipped loc
			, recordMessage = mempty
			}
		ReportAVCheck_ok -> Record
			{ recordScope = scope
			, recordSeverity = SeverityOk
			, recordName = locRecordAVCheckOk loc
			, recordMessage = mempty
			}
		ReportAVCheck_failed e -> Record
			{ recordScope = scope
			, recordSeverity = SeverityBad
			, recordName = locRecordAVCheckFailed loc
			, recordMessage = RichText [RichChunkCode e]
			}
	-- checks related to unpack
	unpackCheckRecords = case report_unpack report of
		ReportUnpack_notStarted -> [Record
			{ recordScope = scope
			, recordSeverity = SeverityInfo
			, recordName = locRecordUnpackNotStarted loc
			, recordMessage = mempty
			}]
		ReportUnpack_succeeded {} ->
			[ binariesPlatformsCheckRecord
			]
		ReportUnpack_failed e -> [Record
			{ recordScope = scope
			, recordSeverity = SeverityErr
			, recordName = locRecordUnpackFailed loc
			, recordMessage = RichText [RichChunkCode e]
			}]
	-- root entries of an upload
	uploadEntries = case report of
		Report
			{ report_unpack = ReportUnpack_succeeded entries
			} -> entries
		_ -> M.empty
	-- check for binaries matching platform tags
	binariesPlatformsCheckRecord =
		if HS.null binariesPlatforms then Record
			{ recordScope = scope
			, recordSeverity = SeverityInfo
			, recordName = locRecordNoBinaries loc
			, recordMessage = locMessageNoBinaries loc
			}
		else if binariesPlatforms == tagsPlatforms then Record
			{ recordScope = scope
			, recordSeverity = SeverityOk
			, recordName = locRecordBinariesCoverPlatforms loc
			, recordMessage = locMessageBinariesCoverPlatforms loc
			}
		else Record
			{ recordScope = scope
			, recordSeverity = SeverityBad
			, recordName = locRecordBinariesPlatformsMismatch loc
			, recordMessage = locMessageBinariesPlatformsMismatch loc
			}
	tagsPlatforms = HS.fromList $ concat
		[ if tagWindows then [PlatformWindows] else []
		, if tagLinux then [PlatformLinux] else []
		, if tagMacOS then [PlatformMacOS] else []
		, if tagAndroid then [PlatformAndroid] else []
		]
	binariesPlatforms = foldEntriesPlatforms uploadEntries HS.empty
	foldEntriesPlatforms entries platforms = foldr foldEntryPlatform platforms $ M.toList entries
	foldEntryPlatform (_entryName, entry) platforms = case entry of
		ReportEntry_file
			{ reportEntry_parses = parses
			} -> foldr foldParsePlatform platforms parses
		ReportEntry_directory
			{ reportEntry_entries = entries
			} -> foldEntriesPlatforms entries platforms
		_ -> platforms
	foldParsePlatform parse platforms = case parse of
		ReportParse_binaryPe
			{ -- reportBinaryPe_arch = arch
			} -> HS.insert PlatformWindows platforms
		ReportParse_binaryElf
			{ -- reportBinaryElf_arch = arch
			} -> HS.insert PlatformLinux platforms
		ReportParse_binaryMachO
			{ -- reportBinary_binaries = subBinaries
			} -> HS.insert PlatformMacOS platforms
		_ -> platforms

analyseUploadGroup :: Localization -> [AnalysisUpload] -> AnalysisUploadGroup
analyseUploadGroup loc uploads = AnalysisUploadGroup
	{ analysisUploadGroup_uploads = uploads
	, analysisUploadGroup_platformsArchs = HS.empty
	}

analyseGame :: Localization -> ItchGame -> [(ItchUpload, Report)] -> AnalysisGame
analyseGame loc _game uploads = AnalysisGame
	{ analysisGame_uploads = analysisUploads
	, analysisGame_release = analyseUploadGroup loc releaseGroup
	, analysisGame_preorder = analyseUploadGroup loc preorderGroup
	, analysisGame_demo = analyseUploadGroup loc demoGroup
	, analysisGame_records = records
	} where
	analysisUploads = flip map uploads $ \(itchUpload, report) -> analyseUpload loc itchUpload report
	releaseGroup = flip filter analysisUploads $ \case
		AnalysisUpload
			{ analysisUpload_itchUpload = ItchUpload
				{ itchUpload_demo = False
				, itchUpload_preorder = False
				}
			} -> True
		_ -> False
	preorderGroup = flip filter analysisUploads $ \case
		AnalysisUpload
			{ analysisUpload_itchUpload = ItchUpload
				{ itchUpload_preorder = True
				}
			} -> True
		_ -> False
	demoGroup = flip filter analysisUploads $ \case
		AnalysisUpload
			{ analysisUpload_itchUpload = ItchUpload
				{ itchUpload_demo = True
				, itchUpload_preorder = False
				}
			} -> True
		_ -> False
	records = noUploadsCheckRecords
	-- check that there're some uploads
	noUploadsCheckRecords =
		if null uploads then [Record
			{ recordScope = GameScope
			, recordSeverity = SeverityBad
			, recordName = locRecordNoUploads loc
			, recordMessage = mempty
			}]
		else []
