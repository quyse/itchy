{-|
Module: Itchy.Localization.En
Description: English localization.
-}

{-# LANGUAGE OverloadedStrings #-}

module Itchy.Localization.En
	( localizationEn
	) where

import Data.Monoid
import qualified Data.Text as T

import Itchy.Localization

localizationEn :: Localization
localizationEn = Localization
	{ locLanguageName = "English"
	, locDashboard = "Dashboard"
	, locGames = "Games"
	, locReports = "Reports"
	, locReportWithNumber = ("Report " <>) . T.pack . show
	, locGeneralError = ("Unknown error: " <>)
	, locDownloadFailed = ("Download failed: " <>)
	, locAVCheckOk = "Anti-virus check is OK"
	, locAVCheckFailed = ("Anti-virus check failed: " <>)
	, locUnpackFailed = ("Unpack failed: " <>)
	}
