{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main(main) where

import Control.Exception
import Data.Monoid
import qualified Data.Aeson.Types as A
import Data.Int
import Data.String
import qualified Data.Text as T
import qualified Data.Yaml as Y
import GHC.Generics(Generic)
import qualified Network.HTTP.Client.TLS as H
import qualified Network.Wai.Application.Static as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Autohead as W
import qualified Network.Wai.Middleware.Gzip as W
import qualified Network.Wai.Middleware.RequestLogger as W
import qualified Options.Applicative as O
import qualified Wai.Routes as W

import Flaw.Book

import Itchy.Itch
import Itchy.ItchCache
import Itchy.ItchInvestigator
import Itchy.Routes
import Itchy.Static

main :: IO ()
main = run =<< O.execParser parser where
	parser = O.info (O.helper <*> opts)
		(  O.fullDesc
		<> O.progDesc "Run Itchy"
		<> O.header "itchy"
		)
	opts = Options
		<$> O.strOption
			(  O.long "config"
			<> O.short 'c'
			<> O.value "itchy.yaml"
			<> O.metavar "CONFIG"
			<> O.help "Config file"
			)

data Options = Options
	{ optionsConfigFileName :: !String
	}

data Config = Config
	{ config_host :: !T.Text
	, config_port :: !Int
	, config_dbFileName :: !T.Text
	, config_itchApiKey :: !T.Text
	, config_itchApiCooldown :: !Int
	, config_cacheStalePeriod :: !Int64
	, config_investigationStalePeriod :: !Int64
	, config_investigatorTempTemplate :: !T.Text
	, config_investigatorDockerTempTemplate :: !T.Text
	, config_investigatorThreadsCount :: !Int
	} deriving Generic

instance A.FromJSON Config where
	parseJSON = A.genericParseJSON A.defaultOptions
		{ A.fieldLabelModifier = drop 7
		}

run :: Options -> IO ()
run Options
	{ optionsConfigFileName = configFileName
	} = withBook $ \bk -> do
	httpManager <- H.getGlobalManager

	eitherConfig <- Y.decodeFileEither configFileName
	Config
		{ config_host = host
		, config_port = port
		, config_dbFileName = dbFileName
		, config_itchApiKey = itchApiKey
		, config_itchApiCooldown = itchApiCooldown
		, config_cacheStalePeriod = cacheStalePeriod
		, config_investigationStalePeriod = investigationStalePeriod
		, config_investigatorTempTemplate = investigatorTempTemplate
		, config_investigatorDockerTempTemplate = investigatorDockerTempTemplate
		, config_investigatorThreadsCount = investigatorThreadsCount
		} <- case eitherConfig of
		Right config -> return config
		Left err -> throwIO err

	itchApi <- book bk $ newItchApi httpManager itchApiKey
	itchCache <- book bk $ newItchCache itchApi dbFileName cacheStalePeriod itchApiCooldown
	itchInvestigator <- book bk $ newItchInvestigator itchCache itchApiKey investigatorTempTemplate investigatorDockerTempTemplate investigatorThreadsCount investigationStalePeriod

	logger <- W.mkRequestLogger $ W.def
		{ W.outputFormat = W.Apache W.FromFallback
		, W.autoFlush = False
		}
	Warp.runSettings
		( Warp.setHost (fromString $ T.unpack host)
		$ Warp.setPort port
		Warp.defaultSettings
		) $ W.gzip W.def
		{ W.gzipFiles = W.GzipCompress
		} $ logger $ W.autohead $ W.waiApp $ do
		W.route App
			{ appItchApi = itchApi
			, appItchCache = itchCache
			, appItchInvestigator = itchInvestigator
			, appItchInvestigationStalePeriod = investigationStalePeriod
			}
		W.catchall $ W.staticApp staticSettings
