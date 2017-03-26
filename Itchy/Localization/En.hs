{-|
Module: Itchy.Localization.En
Description: English localization.
-}

{-# LANGUAGE OverloadedStrings #-}

module Itchy.Localization.En
	( localizationEn
	) where

import Data.Monoid

import Itchy.Localization

localizationEn :: Localization
localizationEn = Localization
	{ locLanguageName = "English"
	, locDashboard = "Dashboard"
	, locGames = "Games"
	, locReports = "Reports"
	, locGameByAuthor = \game author -> game <> " by " <> author
	, locPlatforms = "Platforms"
	, locHasDemo = "Has demo"
	, locNoDemo = "No demo"
	, locFreeDonationsAllowed = "Free, donations allowed"
	, locMinimumPrice = "Minimum price"
	, locFreePaymentsDisabled = "Free, payments disabled"
	, locOptedInPressSystem = "Opted into itch.io press system"
	, locNotOptedInPressSystem = "Did not opted into itch.io press system"
	, locUploads = "Uploads"
	, locDisplayName = "Display name"
	, locFileName = "File name"
	, locSize = "Size"
	, locTags = "Tags"
	, locGeneralError = ("Unknown error: " <>)
	, locDownloadFailed = ("Download failed: " <>)
	, locAVCheckOk = "Anti-virus check is OK"
	, locAVCheckFailed = ("Anti-virus check failed: " <>)
	, locUnpackFailed = ("Unpack failed: " <>)
	}
