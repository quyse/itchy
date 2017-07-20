{-|
Module: Itchy.Localization.Ru
Description: Russian localization.
-}

{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Itchy.Localization.Ru
	( localizationRu
	) where

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Numeric

import Itchy.Localization
import Itchy.Localization.RichText
import Itchy.Report
import Itchy.Report.Record

localizationRu :: Localization
localizationRu = Localization
	{ locLanguageName = "Русский"
	, locHome = "Ковырятор itch.io"
	, locWelcome = "Ковырятор itch.io говорит вам привет! Попробуйте найти вашу игру, введя несколько ключевых слов, и дождитесь появления отчёта Ковырятора. Внимание: это альфа версия, может показывать странное. Данный сервис неофициальный, и никак не аффилирован с itch.io."
	, locSearch = "Поиск"
	, locGameByAuthor = \game author -> game <> " от " <> author
	, locDescription = \desc -> "Описание: " <> desc
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
	, locAccessMode = "Доступ"
	, locSize = "Размер"
	, locTags = "Теги"
	, locSizeInBytes = \size ->
		if size < 2 * 1024 then T.pack (show size) <> " б"
		else if size < 2 * 1024 * 1024 then T.pack (showFFloat (Just 1) (fromIntegral size / 1024 :: Float) "") <> " Кб"
		else if size < 2 * 1024 * 1024 * 1024 then T.pack (showFFloat (Just 1) (fromIntegral size / (1024 * 1024) :: Float) "") <> " Мб"
		else T.pack (showFFloat (Just 1) (fromIntegral size / (1024 * 1024 * 1024) :: Float) "") <> " Гб"
	, locSymlink = "симв. ссылка"
	, locNoUserVersion = "не указана"
	, locBuildVersion = \buildVersion buildUserVersion -> "версия: " <> RichText [RichChunkCode buildVersion] <> ", пользовательская версия: " <> RichText [RichChunkCode buildUserVersion]
	, locDoesntUseButler = "не использует butler"
	, locInvestigationStarted = "ставим в очередь"
	, locInvestigationQueued = \n -> T.pack ("номер " ++ shows n " в очереди")
	, locInvestigationProcessing = "обрабатывается"
	, locInvestigationSucceeded = "готов"
	, locInvestigationFailed = "ошибка"
	, locReinvestigate = "Обработать ещё раз"
	, locReport = "Отчёт"
	, locReportNotComplete = \a b -> "Отчёт неполон! Обработано " <> RichText [RichChunkCode $ T.pack $ show a] <> " " <> plural a "пакет" "пакета" "пакетов" <> " из " <> RichText [RichChunkCode $ T.pack $ show b] <> "."
	, locRecordSeverity = "Статус"
	, locRecordScope = "Субъект"
	, locRecordName = "Тест"
	, locRecordMessage = "Пояснение"
	, locScopeProject = "Проект"
	, locScopeUploadGroup = \case
		UploadGroupRelease -> "Релизные пакеты"
		UploadGroupPreorder -> "Предзаказ-пакеты"
		UploadGroupDemo -> "Демо пакеты"
	, locScopeUpload = \maybeUpload -> RichText [RichChunkCode $ fromMaybe "<?>" maybeUpload]
	, locScopeEntry = \maybeUpload entry -> RichText [RichChunkCode (fromMaybe "<?>" maybeUpload), RichChunkText ": ", RichChunkCode entry]
	, locUnknownGame = "Неизвестная игра"
	, locGameNotCached = "Запрашиваем информацию по этой игре... Страница обновится через несколько секунд."
	, locRefresh = "Обновить"
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
	, locRecordDepVersionRequirement = \distroName -> RichText ["Требуется версия ", RichChunkCode distroName, " или выше"]
	, locMessageDepVersionRequirement = \depName (reportDepVersionToText -> depVersion) distroName (reportDepVersionToText -> distroVersion) -> RichText
		[ "Требуемая версия ", RichChunkCode depName, " - ", RichChunkCode depVersion
		, " для которой в свою очередь требуется дистрибутив не ниже ", RichChunkCode distroName
		, " (содержащий ", RichChunkCode depName, " ", RichChunkCode distroVersion, ")."
		]
	, locRecordNoBinaries = "Бинарники не найдены"
	, locMessageNoBinaries = "В данном пакете не найдено ни одного исполняемого файла. Это нормально для неисполняемого пакета, такого как книга, саундтрек, пак ассетов. В противном случае это, вероятно, ошибка."
	, locRecordBinariesCoverPlatforms = "Есть бинарники для всех указанных платформ"
	, locMessageBinariesCoverPlatforms = "Исполняемые файлы были найдены для каждой платформы, указанной в тегах пакета."
	, locRecordUntaggedBinary = \platform -> "Нет тега " <> RichText [RichChunkCode platform]
	, locMessageUntaggedBinary = \platform -> "В пакете есть бинарник для " <> RichText [RichChunkText platform] <> ", но сам пакет не имеет тега " <> RichText [RichChunkCode platform] <> ". Вероятно, вы забыли поставить тег для пакета, или наоборот, положили неправильный бинарник."
	, locRecordNoPlatformBinaries = \platform -> "Нет бинарников для " <> RichText [RichChunkText platform]
	, locMessageNoPlatformBinaries = \platform -> "Пакет отмечен тегом " <> RichText [RichChunkCode platform] <> ", но не содержит бинарников для " <> RichText [RichChunkText platform] <> ". Если ваша игра и не предполагает наличия исполняемых файлов, не обращайте внимания на это сообщение. В противном случае вы вероятно забыли положить бинарник в пакет, или положили неправильный. Или задали лишний тег пакету."
	, locRecordWindowsBinaryX86Exists = "Есть 32-битная версия для Windows"
	, locRecordNoWindowsBinaryX86 = "Нет 32-битной версии для Windows"
	, locMessageAboutWindowsBinaryX86 = "64-битные исполняемые файлы не могут быть запущены на 32-битных Windows-системах, которые всё ещё встречаются у игроков. Рекомендуется предоставлять 32-битный исполняемый файл, который спокойно работает и на 32-х, и на 64-битных системах."
	, locRecordNoLinuxBinaryX64 = "Нет 64-битной версии для Linux"
	, locRecordNoLinuxBinaryX86 = "Нет 32-битной версии для Linux"
	, locRecordHasLinuxBinaryX64X86 = "Есть и 64, и 32-битная версии для Linux"
	, locMessageAboutLinuxBinaryArchs = "Рекомендуется предоставлять как 64-битную, так и 32-битную версию исполняемых файлов на Linux. Сейчас большинство Linux систем 64-битные. На 64-битном Linux, в отличие от других ОС, 32-битные программы обычно не запускаются \"из коробки\", и может потребоваться поставить дополнительные библиотеки (\"multilib\"). Также запуск 64-битных программ невозможен на 32-битных Linux-системах, которые всё ещё встречаются у игроков."
	, locRecordMacOSBinaryX86Exists = "Есть 32-битная версия для macOS"
	, locRecordNoMacOSBinaryX86 = "Нет 32-битной версии для macOS"
	, locMessageAboutMacOSBinaryX86 = "Запуск 64-битных программ невозможен на 32-битных macOS-системах, которые всё ещё используются. Рекомендуется предоставлять 32-битный исполняемый файл вдобавок к 64-битному, или единый универсальный исполняемый файл, поддерживающий обе архитектуры."
	, locRecordHasDemo = "Есть демо-версия"
	, locRecordNoDemo = "Нет демо-версии"
	, locMessageAboutDemo = "Вы можете добавить демо-версию, чтобы игроки могли лучше оценить вашу игру перед покупкой."
	, locRecordOptedIntoPressSystem = "Входит в программу доступа для прессы itch.io"
	, locRecordNotOptedIntoPressSystem = "Не входит в программу доступа для прессы itch.io"
	, locMessageAboutPressSystem = "Вы можете включить игры вашего аккаунта в " <> RichText [RichChunkLink "https://itch.io/press/user-list" "программу доступа для прессы itch.io"] <> ", позволяющую проверенным аккаунтам от игровой прессы загружать и играть в ваши игры бесплатно."
	, locRecordNoUploads = "Нет пакетов"
	}

plural :: Integral n => n -> a -> a -> a -> a
plural ((`rem` 100) . abs -> nn@((`rem` 10) -> n)) one few other =
	if nn >= 10 && nn <= 19 then other
	else if n == 1 then one
	else if n > 1 && n < 5 then few
	else other
