{-|
Module: Itchy.Report.Analysis
Description: Report analysis structures.
-}

{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

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
	{ itchUpload_id = uploadId@(UploadScope -> scope)
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
		++ glibcVersionCheckRecords
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
	distroVersions =
		[ (("Ubuntu 12.04 \"precise\"", SeverityOk), ReportDepVersion [2, 15])
		, (("Ubuntu 14.04 \"trusty\"", SeverityWarn), ReportDepVersion [2, 19])
		, (("Ubuntu 16.04 \"xenial\"", SeverityWarn), ReportDepVersion [2, 23])
		, (("Ubuntu 16.10 \"yakkety\"", SeverityWarn), ReportDepVersion [2, 24, 3])
		, (("Ubuntu 17.04 \"zesty\"", SeverityWarn), ReportDepVersion [2, 24, 7])
		]
	-- checks related to unpack
	unpackCheckRecords = case report_unpack report of
		ReportUnpack_notStarted -> [Record
			{ recordScope = scope
			, recordSeverity = SeverityInfo
			, recordName = locRecordUnpackNotStarted loc
			, recordMessage = mempty
			}]
		ReportUnpack_succeeded {} -> binariesPlatformsCheckRecords
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
	binariesPlatformsCheckRecords =
		if HS.null binariesPlatforms && HS.null tagsPlatforms then []
		else if HS.null binariesPlatforms then [Record
			{ recordScope = scope
			, recordSeverity = SeverityWarn
			, recordName = locRecordNoBinaries loc
			, recordMessage = locMessageNoBinaries loc
			}]
		else if binariesOnlyPlatforms == tagsPlatforms then [Record
			{ recordScope = scope
			, recordSeverity = SeverityOk
			, recordName = locRecordBinariesCoverPlatforms loc
			, recordMessage = locMessageBinariesCoverPlatforms loc
			}]
		else concat $ flip map (HS.toList $ HS.union binariesOnlyPlatforms tagsPlatforms) $ \platform@(platformName -> pn) ->
			case (HS.member platform binariesOnlyPlatforms) `compare` (HS.member platform tagsPlatforms) of
				EQ -> []
				LT -> [Record
					{ recordScope = scope
					, recordSeverity = SeverityWarn
					, recordName = locRecordNoPlatformBinaries loc pn
					, recordMessage = locMessageNoPlatformBinaries loc pn
					}]
				GT -> [Record
					{ recordScope = scope
					, recordSeverity = SeverityBad
					, recordName = locRecordUntaggedBinary loc pn
					, recordMessage = locMessageUntaggedBinary loc pn
					}]
	tagsPlatforms = HS.fromList $ concat
		[ if tagWindows then [PlatformWindows] else []
		, if tagLinux then [PlatformLinux] else []
		, if tagMacOS then [PlatformMacOS] else []
		, if tagAndroid then [PlatformAndroid] else []
		]
	binariesPlatforms@(HS.map fst -> binariesOnlyPlatforms) = foldEntriesPlatforms uploadEntries HS.empty
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
	-- GLIBC version check
	glibcVersionCheckRecords =
		if HS.member PlatformLinux tagsPlatforms then entriesGlibcVersionCheckRecords
		else []
	entriesGlibcVersionCheckRecords = foldEntriesGlibcCheckRecords uploadEntries [] []
	foldEntriesGlibcCheckRecords entries entriesPath records = let
		foldEntryGlibcCheckRecords (entryName, entry) records = let
			entryPath = entriesPath ++ [entryName]
			entryScope = EntryScope uploadId entryPath
			foldParseGlibcCheckRecords parse records = case parse of
				ReportParse_binaryElf ReportBinaryElf
					{ reportBinaryElf_deps = deps
					} -> case filter (("GLIBC" ==) . reportDep_name) deps of
					(ReportDep
						{ reportDep_version = glibcVersion
						} : _) -> foldDepGlibcCheckRecords glibcVersion records
					[] -> records
				_ -> records
			foldDepGlibcCheckRecords glibcVersion records = case dropWhile ((< glibcVersion) . snd) distroVersions of
				(((distroName, severity), distroGlibcVersion) : _) -> Record
					{ recordScope = entryScope
					, recordSeverity = severity
					, recordName = locRecordDepVersionRequirement loc distroName
					, recordMessage = locMessageDepVersionRequirement loc "GLIBC" glibcVersion distroName distroGlibcVersion
					} : records
				[] -> Record
					{ recordScope = entryScope
					, recordSeverity = SeverityBad
					, recordName = locRecordDepVersionRequirement loc mempty
					, recordMessage = mempty
					} : records
			in case entry of
				ReportEntry_file
					{ reportEntry_parses = parses
					} -> foldr foldParseGlibcCheckRecords records parses
				ReportEntry_directory
					{ reportEntry_entries = entries
					} -> foldEntriesGlibcCheckRecords entries entryPath records
				_ -> records
		in foldr foldEntryGlibcCheckRecords records $ M.toList entries

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
		, if HS.member PlatformLinux groupPlatformsOnly then let
				hasX64 = HS.member (PlatformLinux, ReportArch_x64) groupPlatforms
				hasX86 = HS.member (PlatformLinux, ReportArch_x86) groupPlatforms
				in concat
				[ if hasX64 then [] else [Record
					{ recordScope = scope
					, recordSeverity = SeverityWarn
					, recordName = locRecordNoLinuxBinaryX64 loc
					, recordMessage = locMessageAboutLinuxBinaryArchs loc
					}]
				, if hasX86 then [] else [Record
					{ recordScope = scope
					, recordSeverity = SeverityWarn
					, recordName = locRecordNoLinuxBinaryX86 loc
					, recordMessage = locMessageAboutLinuxBinaryArchs loc
					}]
				, if hasX64 && hasX86 then [Record
					{ recordScope = scope
					, recordSeverity = SeverityOk
					, recordName = locRecordHasLinuxBinaryX64X86 loc
					, recordMessage = locMessageAboutLinuxBinaryArchs loc
					}] else []
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
analyseGame loc ItchGame
	{ itchGame_in_press_system = gameInPressSystem
	, itchGame_has_demo = gameHasDemo
	, itchGame_min_price = gameMinPrice
	} uploads = AnalysisGame
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
	records = paidRecords ++ noUploadsCheckRecords
	paidRecords =
		if gameMinPrice > 0 then [gameHasDemoCheckRecord, gameInPressSystemCheckRecord]
		else []
	gameHasDemoCheckRecord =
		if gameHasDemo then Record
			{ recordScope = ProjectScope
			, recordSeverity = SeverityOk
			, recordName = locRecordHasDemo loc
			, recordMessage = locMessageAboutDemo loc
			}
		else Record
			{ recordScope = ProjectScope
			, recordSeverity = SeverityTip
			, recordName = locRecordNoDemo loc
			, recordMessage = locMessageAboutDemo loc
			}
	-- check that there's press access to game
	gameInPressSystemCheckRecord =
		if gameInPressSystem then Record
			{ recordScope = ProjectScope
			, recordSeverity = SeverityOk
			, recordName = locRecordOptedIntoPressSystem loc
			, recordMessage = locMessageAboutPressSystem loc
			}
		else Record
			{ recordScope = ProjectScope
			, recordSeverity = SeverityTip
			, recordName = locRecordNotOptedIntoPressSystem loc
			, recordMessage = locMessageAboutPressSystem loc
			}
	-- check that there're some uploads
	noUploadsCheckRecords =
		if null uploads then [Record
			{ recordScope = ProjectScope
			, recordSeverity = SeverityInfo
			, recordName = locRecordNoUploads loc
			, recordMessage = mempty
			}]
		else []

platformName :: Platform -> T.Text
platformName = \case
	PlatformWindows -> "Windows"
	PlatformLinux -> "Linux"
	PlatformMacOS -> "MacOS"
	PlatformAndroid -> "Android"
