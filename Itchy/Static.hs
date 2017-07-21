{-|
Module: Itchy.Static
Description: Embedded static files.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Itchy.Static
	( staticSettings
	, staticPath
	) where

import qualified Crypto.Hash as C
import Control.Monad
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(qAddDependentFile)
import Language.Haskell.TH.Quote
import Network.Mime
import System.Directory
import System.Exit
import System.IO
import qualified System.Process as P
import qualified WaiAppStatic.Types as W
import qualified WaiAppStatic.Storage.Embedded as W

staticSettingsAndMapping :: (W.StaticSettings, T.Text -> T.Text)
staticSettingsAndMapping = $(do
	let loadFile process urlDirPath diskDirPath fileName = do
		-- full path on disk
		let diskFullPath = diskDirPath <> "/" <> fileName
		-- make file build dependency
		qAddDependentFile diskFullPath
		-- process file
		fileData <- process diskFullPath
		-- hash file data and Base64-encode it
		--let etag = T.decodeUtf8 $ B64.encode $ SHA1.hashlazy fileData
		let etag = T.decodeUtf8 (BA.convertToBase BA.Base64URLUnpadded $ BA.takeView (C.hashlazy fileData :: C.Digest C.SHA256) 10)

		-- calculate url
		let urlFullPath = T.pack urlDirPath <> "/" <> etag <> "/" <> T.pack fileName

		return (W.EmbeddableEntry
			{ W.eLocation = urlFullPath
			, W.eMimeType = defaultMimeLookup $ T.pack fileName
			, W.eContent = Left (etag, fileData)
			}, (T.pack $ diskFullPath, "/" <> urlFullPath))

	let loadDirectory process urlDirPath diskDirPath = mapM (loadFile process urlDirPath diskDirPath) =<<
		runIO (filterM (doesFileExist . ((diskDirPath ++ "/") ++)) =<< getDirectoryContents diskDirPath)

	-- processing helper function
	let process command diskFullPath = do
		(fileData, exitCode) <- runIO $ withFile diskFullPath ReadMode $ \h -> do
			(Nothing, Just hOut, Nothing, ph) <- P.createProcess (P.shell command)
				{ P.std_in = P.UseHandle h
				, P.std_out = P.CreatePipe
				}
			fileData <- B.hGetContents hOut
			exitCode <- P.waitForProcess ph
			return (fileData, exitCode)
		case exitCode of
			ExitSuccess -> return $ BL.fromStrict fileData
			ExitFailure c -> do
				reportError $ "compilation of static file failed with code " ++ (show c) ++ ": " ++ diskFullPath
				return $ BL.empty

	-- build static files
	(entries, mappings) <- liftM unzip $ liftM concat $ sequence
		[ loadDirectory (runIO . BL.readFile) "static" "static"
		, loadDirectory (process "$(npm bin)/stylus -c -p ") "static" "static-stylus"
		, loadDirectory (process "$(npm bin)/uglifyjs -m -c") "static" "static-js"
		]

	-- log
	runIO $ do
		putStrLn $ "EMBEDDING FILES"
		forM_ mappings $ \(u, d) -> putStrLn $ T.unpack $ "\t" <> u <> "\t" <> d

	let settingsExp = W.mkSettings $ return entries
	x <- newName "x"
	let mappingExp = let
		mappingMatch (u, d) = match (litP $ stringL $ T.unpack u) (normalB $ litE $ stringL $ T.unpack d) []
		in lamE [varP x] $ caseE (varE x) $ map mappingMatch mappings ++ [match wildP (normalB [| error "unknown mapping" |]) []]

	[| ($settingsExp
		{ W.ssMaxAge = W.MaxAgeForever
		}, $mappingExp)
		|]
	)

staticSettings :: W.StaticSettings
staticSettings = fst staticSettingsAndMapping

staticMapping :: T.Text -> T.Text
staticMapping = snd staticSettingsAndMapping

staticPath :: QuasiQuoter
staticPath = QuasiQuoter
	{ quoteExp = \fileName -> do
		qAddDependentFile fileName
		litE $ stringL $ T.unpack $ staticMapping $ T.pack fileName
	, quotePat = undefined
	, quoteType = undefined
	, quoteDec = undefined
	}
