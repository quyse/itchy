{-|
Module: Itchy.Localization.Ru
Description: Russian localization.
-}

{-# LANGUAGE OverloadedStrings #-}

module Itchy.Localization.Ru
	( localizationRu
	) where

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Numeric

import Itchy.Localization

localizationRu :: Localization
localizationRu = Localization
	{ locLanguageName = "Русский"
	, locDashboard = "Кабинет"
	, locGames = "Игры"
	, locReports = "Отчёты"
	, locGameByAuthor = \game author -> game <> " от " <> author
	, locPlatforms = "Платформы"
	, locHasDemo = "Есть демо"
	, locNoDemo = "Нет демо"
	, locFreeDonationsAllowed = "Бесплатно, пожертвования разрешены"
	, locMinimumPrice = "Минимальная цена"
	, locFreePaymentsDisabled = "Бесплатно, пожертвования отключены"
	, locOptedInPressSystem = "Входит в программу доступа для прессы itch.io"
	, locNotOptedInPressSystem = "Не входит в программу доступа для прессы itch.io"
	, locUploads = "Пакеты"
	, locDisplayName = "Отображаемое имя"
	, locFileName = "Имя файла"
	, locSize = "Размер"
	, locTags = "Теги"
	, locSizeInBytes = \size ->
		if size < 2 * 1024 then T.pack (show size) <> " б"
		else if size < 2 * 1024 * 1024 then T.pack (showFFloat (Just 1) (fromIntegral size / 1024 :: Float) "") <> " Кб"
		else if size < 2 * 1024 * 1024 * 1024 then T.pack (showFFloat (Just 1) (fromIntegral size / (1024 * 1024) :: Float) "") <> " Мб"
		else T.pack (showFFloat (Just 1) (fromIntegral size / (1024 * 1024 * 1024) :: Float) "") <> " Гб"
	, locReport = "Отчёт"
	, locRecordSeverity = "Важность"
	, locRecordScope = "Субъект"
	, locRecordName = "Тест"
	, locRecordMessage = "Комментарий"
	, locScopeUpload = \maybeUpload -> RichText [RichChunkCode $ fromMaybe "<?>" maybeUpload]
	, locScopeEntry = \maybeUpload entry -> RichText [RichChunkCode (fromMaybe "<?>" maybeUpload), RichChunkText ": ", RichChunkCode entry]
	, locSeverityOk = "OK"
	, locSeverityInfo = "ИНФО"
	, locSeverityTip = "СОВЕТ"
	, locSeverityWarn = "ПРЕД"
	, locSeverityBad = "ЖОПА"
	, locSeverityErr = "ПРОБЛ"
	, locRecordUploadDisplayNameSet = "Отображаемое имя задано"
	, locRecordUploadDisplayNameNotSet = "Отображаемое имя не задано"
	, locMessageUploadDisplayNameNotSet = "Сейчас пакет отображается в виде его имени файла, но вы можете задать любое отображаемое название."
	, locRecordUnknownError = "Неизвестная ошибка"
	, locRecordAVCheckNotStarted = "Антивирусная проверка не начиналась"
	, locRecordAVCheckSkipped = "Антивирусная проверка пропущена"
	, locRecordAVCheckOk = "Антивирусная проверка успешна"
	, locRecordAVCheckFailed = "Антивирусная проверка провалена"
	, locRecordUnpackNotStarted = "Распаковка не выполнена"
	, locRecordUnpackFailed = "Распаковка завершилась с ошибкой"
	, locRecordNoBinaries = "Бинарники не найдены"
	, locMessageNoBinaries = "В данном пакете не найдено ни одного исполняемого файла. Это нормально для неисполняемого пакета, такого как книга, саундтрек, пак ассетов."
	, locRecordBinariesCoverPlatforms = "Есть бинарники для всех указанных платформ"
	, locMessageBinariesCoverPlatforms = "Исполняемые файлы были найдены для каждой платформы, указанной в тегах пакета."
	, locRecordBinariesPlatformsMismatch = "Бинарники и теги не совпадают"
	, locMessageBinariesPlatformsMismatch = "Бинарники и теги не совпадают"
	, locRecordNoUploads = "Нет пакетов"
	}
