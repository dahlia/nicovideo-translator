{-# LANGUAGE OverloadedStrings #-}
module NicovideoTranslator.Main (main) where

import Data.Char (toLower)
import System.IO (stderr, stdout)
import System.IO.Error (catchIOError)

import Data.LanguageCodes (ISO639_1(EN), fromChars)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (hPutStrLn)
import Network.DNS.Resolver (defaultResolvConf, makeResolvSeed, withResolver)
import Network.DNS.Lookup (lookupA)
import Network.Wai.Handler.Warp (run)
import System.Locale.SetLocale (Category(LC_CTYPE), setLocale)

import NicovideoTranslator.Proxy (ProxyConfiguration(ProxyConfiguration), app)

language :: IO ISO639_1
language = do
    currentLocale <- setLocale LC_CTYPE Nothing
    case currentLocale of
        Just (x:y:ys) -> case fromChars (toLower x) (toLower y) of
            Just lang -> return lang
            _ -> ioError $ userError $ (x:y:ys) ++ " is wrong LC_CTYPE value"
        _ -> ioError $ userError "locale is not set"

defaultUpstreamHost :: T.Text
defaultUpstreamHost = "msg.nicovideo.jp"

main :: IO ()
main = do
    putStrLn "Running on http://0.0.0.0:80/ (Press ^C to quit)"
    lang <- catchIOError language (\_ -> return EN)
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
                run 80 $ app (ProxyConfiguration lang upstreamHost)
        _ -> hPutStrLn stderr $ T.concat [ "error: failed to resolve "
                                         , defaultUpstreamHost
                                         ]
