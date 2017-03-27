{-|
Module: Itchy.Localization.Ru
Description: Russian localization.
-}

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Itchy.Localization.Ru
	( localizationRu
	) where

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Numeric

import Itchy.Localization
import Itchy.Localization.RichText
import Itchy.Report.Record

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
	, locScopeUploadGroup = \case
		UploadGroupRelease -> "Релизные пакеты"
		UploadGroupPreorder -> "Предзаказ-пакеты"
		UploadGroupDemo -> "Демо пакеты"
	, locScopeUpload = \maybeUpload -> RichText [RichChunkCode $ fromMaybe "<?>" maybeUpload]
	, locScopeEntry = \maybeUpload entry -> RichText [RichChunkCode (fromMaybe "<?>" maybeUpload), RichChunkText ": ", RichChunkCode entry]
	, locSeverityOk = "НОРМ"
	, locSeverityInfo = "ИНФО"
	, locSeverityTip = "СОВЕТ"
	, locSeverityWarn = "ХММ..."
	, locSeverityBad = "ЖОПА"
	, locSeverityErr = "ТРАБЛ"
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
	, locRecordWindowsBinaryX86Exists = "Есть 32-битная версия для Windows"
	, locRecordNoWindowsBinaryX86 = "Нет 32-битной версии для Windows"
	, locMessageAboutWindowsBinaryX86 = "64-битные исполняемые файлы не могут быть запущены на 32-битных Windows-системах, которые всё ещё встречаются у игроков. Лучше предоставлять 32-битный исполняемый файл, который спокойно работает и на 32-х, и на 64-битных системах."
	, locRecordNoLinuxBinaryX64 = "Нет 64-битной версии для Linux"
	, locMessageNoLinuxBinaryX64 = "Сейчас большинство Linux систем 64-битные. На 64-битном Linux, в отличие от других ОС, 32-битные программы обычно не запускаются \"из коробки\", и может потребоваться поставить дополнительные библиотеки (\"multilib\"). Рекомендуется предоставлять 64-битную версию вдобавок к 32-битной, чтобы не создавать лишних трудностей большинству игроков."
	, locRecordNoLinuxBinaryX86 = "Нет 32-битной версии для Linux"
	, locMessageNoLinuxBinaryX86 = "Запуск 64-битных программ невозможен на 32-битных Linux-системах, которые всё ещё встречаются у игроков. Рекомендуется предоставлять 32-битный исполняемый файл вдобавок к 64-битному."
	, locRecordMacOSBinaryX86Exists = "Есть 32-битная версия для macOS"
	, locRecordNoMacOSBinaryX86 = "Нет 32-битной версии для macOS"
	, locMessageAboutMacOSBinaryX86 = "Запуск 64-битных программ невозможен на 32-битных macOS-системах, которые всё ещё используются. Рекомендуется предоставлять 32-битный исполняемый файл вдобавок к 64-битному, или единый универсальный исполняемый файл, поддерживающий обе архитектуры."
	, locRecordNoUploads = "Нет пакетов"
	}
