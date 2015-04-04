{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module NicovideoTranslator.Main (main) where

import Data.Char (toLower)
import System.IO (stderr, stdout)
import System.IO.Error (catchIOError)

import Data.Data (Data)
import qualified Data.LanguageCodes as L
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format (Format, format)
import Data.Text.Format.Params (Params)
import Data.Text.Format.Types (Only(Only))
import Data.Text.IO (hPutStrLn)
import qualified Data.Text.Lazy as LT
import Data.Typeable (Typeable)
import Network.DNS.Resolver (defaultResolvConf, makeResolvSeed, withResolver)
import Network.DNS.Lookup (lookupA)
import Network.Wai.Handler.Warp (Port, run)
import System.Console.CmdArgs (cmdArgs, help, typ, (&=))
import System.Locale.SetLocale (Category(LC_CTYPE), setLocale)

import NicovideoTranslator.Proxy (ProxyConfiguration(ProxyConfiguration), app)

data Translator = Translator { port :: Port
                             , language :: String
                             } deriving (Show, Data, Typeable)

formatIoError :: Params ps => Format -> ps -> IO a
formatIoError fmt ps = ioError $ userError $ LT.unpack $ format fmt ps 

readLanguageCode :: [Char] -> IO L.ISO639_1
readLanguageCode (x:y:[]) =
    case L.fromChars (toLower x) (toLower y) of
        Just lang -> return lang
        _ -> formatIoError "{} is wrong language code" (Only (x:y:[]))
readLanguageCode lang = formatIoError "{} is wrong language code" (Only lang)

currentLanguage :: IO L.ISO639_1
currentLanguage = do
    currentLocale <- setLocale LC_CTYPE Nothing
    case currentLocale of
        Just s -> readLanguageCode s
        Nothing -> formatIoError "locale is not set" ()

defaultUpstreamHost :: T.Text
defaultUpstreamHost = "msg.nicovideo.jp"

translateCmdArgs :: String -> Translator
translateCmdArgs lang =
    Translator { language = lang &= typ "LANG"
                                 &= help "Target language to translate to [en]"
               , port = 80 &= typ "PORT"
                           &= help "Port number to listen [80]"
               }

main :: IO ()
main = do
    currentLang <- catchIOError currentLanguage (\_ -> return L.EN)
    opts <- cmdArgs $ translateCmdArgs $ L.language currentLang
    lang <- readLanguageCode $ language opts
    let portNum = port opts
    hPutStrLn stdout $ LT.toStrict $
        format "Running on http://0.0.0.0:{}/ (Press ^C to quit)" (Only portNum)
    rs <- makeResolvSeed defaultResolvConf
    resolution <- withResolver rs $
        \resolver -> lookupA resolver (encodeUtf8 defaultUpstreamHost)
    case resolution of
        Right (resolvedHost:_) ->
            let upstreamHost = T.pack $ show resolvedHost
            in do
                hPutStrLn stdout $ T.concat ["Upstream: "
                                            , defaultUpstreamHost
                                            , " (", upstreamHost, ")"
                                            ]
                run portNum $ app (ProxyConfiguration lang upstreamHost)
        _ -> hPutStrLn stderr $ T.concat [ "error: failed to resolve "
                                         , defaultUpstreamHost
                                         ]
