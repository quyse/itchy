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
import Itchy.Localization.RichText
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
	, analysisUpload_binariesPlatforms :: !(HS.HashSet (Platform, ReportArch))
	, analysisUpload_records :: [Record]
	}

data AnalysisUploadGroup = AnalysisUploadGroup
	{ analysisUploadGroup_uploads :: [AnalysisUpload]
	, analysisUploadGroup_platforms :: !(HS.HashSet (Platform, ReportArch))
	, analysisUploadGroup_records :: [Record]
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
		else if HS.map fst binariesPlatforms == tagsPlatforms then Record
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
		ReportParse_binaryPe ReportBinaryPe
			{ reportBinaryPe_arch = arch
			} -> HS.insert (PlatformWindows, arch) platforms
		ReportParse_binaryElf ReportBinaryElf
			{ reportBinaryElf_arch = arch
			} -> HS.insert (PlatformLinux, arch) platforms
		ReportParse_binaryMachO ReportBinaryMachO
			{ reportBinaryMachO_binaries = subBinaries
			} -> foldr HS.insert platforms $ flip map subBinaries $ \ReportMachOSubBinary
			{ reportMachoSubBinary_arch = arch
			} -> (PlatformMacOS, arch)
		_ -> platforms

analyseUploadGroup :: Localization -> UploadGroup -> [AnalysisUpload] -> AnalysisUploadGroup
analyseUploadGroup loc (UploadGroupScope -> scope) uploads = AnalysisUploadGroup
	{ analysisUploadGroup_uploads = uploads
	, analysisUploadGroup_platforms = groupPlatforms
	, analysisUploadGroup_records = groupRecords
	} where
	groupPlatforms = foldr HS.union HS.empty $ map analysisUpload_binariesPlatforms uploads
	groupPlatformsOnly = HS.map fst groupPlatforms
	groupRecords = concat
		[ if HS.member PlatformWindows groupPlatformsOnly then
				if HS.member (PlatformWindows, ReportArch_x86) groupPlatforms then [Record
					{ recordScope = scope
					, recordSeverity = SeverityOk
					, recordName = locRecordWindowsBinaryX86Exists loc
					, recordMessage = locMessageAboutWindowsBinaryX86 loc
					}]
				else [Record
					{ recordScope = scope
					, recordSeverity = SeverityWarn
					, recordName = locRecordNoWindowsBinaryX86 loc
					, recordMessage = locMessageAboutWindowsBinaryX86 loc
					}]
			else []
		, if HS.member PlatformLinux groupPlatformsOnly then concat
				[ if HS.member (PlatformLinux, ReportArch_x64) groupPlatforms then [] else [Record
					{ recordScope = scope
					, recordSeverity = SeverityWarn
					, recordName = locRecordNoLinuxBinaryX64 loc
					, recordMessage = locMessageNoLinuxBinaryX64 loc
					}]
				, if HS.member (PlatformLinux, ReportArch_x86) groupPlatforms then [] else [Record
					{ recordScope = scope
					, recordSeverity = SeverityWarn
					, recordName = locRecordNoLinuxBinaryX86 loc
					, recordMessage = locMessageNoLinuxBinaryX86 loc
					}]
				]
			else []
		, if HS.member PlatformMacOS groupPlatformsOnly then
				if HS.member (PlatformMacOS, ReportArch_x86) groupPlatforms then [Record
					{ recordScope = scope
					, recordSeverity = SeverityOk
					, recordName = locRecordMacOSBinaryX86Exists loc
					, recordMessage = locMessageAboutMacOSBinaryX86 loc
					}]
				else [Record
					{ recordScope = scope
					, recordSeverity = SeverityWarn
					, recordName = locRecordNoMacOSBinaryX86 loc
					, recordMessage = locMessageAboutMacOSBinaryX86 loc
					}]
			else []
		]

analyseGame :: Localization -> ItchGame -> [(ItchUpload, Report)] -> AnalysisGame
analyseGame loc _game uploads = AnalysisGame
	{ analysisGame_uploads = analysisUploads
	, analysisGame_release = analyseUploadGroup loc UploadGroupRelease releaseGroup
	, analysisGame_preorder = analyseUploadGroup loc UploadGroupPreorder preorderGroup
	, analysisGame_demo = analyseUploadGroup loc UploadGroupDemo demoGroup
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
