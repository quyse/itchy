{-|
Module: Itchy.Localization.Ru
Description: Russian localization.
-}

{-# LANGUAGE OverloadedStrings #-}

module Itchy.Localization.Ru
	( localizationRu
	) where

import Data.Monoid
import qualified Data.Text as T

import Itchy.Localization

localizationRu :: Localization
localizationRu = Localization
	{ locLanguageName = "Русский"
	, locDashboard = "Кабинет"
	, locGames = "Игры"
	, locReports = "Отчёты"
	, locReportWithNumber = ("Отчёт " <>) . T.pack . show
	, locGeneralError = ("Неизвестная ошибка: " <>)
	, locDownloadFailed = ("Загрузка не удалась: " <>)
	, locAVCheckOk = "Антивирусная проверка пройдена"
	, locAVCheckFailed = ("Антивирусная проверка провалена: " <>)
	, locUnpackFailed = ("Распаковка не удалась: " <>)
	}
