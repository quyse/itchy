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
	, locGameByAuthor = \game author -> game <> " от " <> author
	, locPlatforms = "Платформы"
	, locHasDemo = "Есть демо"
	, locNoDemo = "Нет демо"
	, locFreeDonationsAllowed = "Бесплатно, пожертвования разрешены"
	, locMinimumPrice = "Минимальная цена"
	, locFreePaymentsDisabled = "Бесплатно, пожертвования отключены"
	, locOptedInPressSystem = "Входит в программу доступа для прессы itch.io"
	, locNotOptedInPressSystem = "Не входит в программу доступа для прессы itch.io"
	, locUploads = "Загрузки"
	, locDisplayName = "Отображаемое имя"
	, locFileName = "Имя файла"
	, locSize = "Размер"
	, locTags = "Теги"
	, locGeneralError = ("Неизвестная ошибка: " <>)
	, locDownloadFailed = ("Загрузка не удалась: " <>)
	, locAVCheckOk = "Антивирусная проверка пройдена"
	, locAVCheckFailed = ("Антивирусная проверка провалена: " <>)
	, locUnpackFailed = ("Распаковка не удалась: " <>)
	}
