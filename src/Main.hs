module Main (main) where

import Data.Char (toLower)
import System.IO.Error (catchIOError)

import Data.LanguageCodes (ISO639_1(EN), fromChars)
import Network.Wai.Handler.Warp (run)
import System.Locale.SetLocale (Category(LC_CTYPE), setLocale)

import NicovideoTranslator.Proxy (app)

language :: IO ISO639_1
language = do
    currentLocale <- setLocale LC_CTYPE Nothing
    case currentLocale of
        Just (x:y:ys) -> case fromChars (toLower x) (toLower y) of
            Just lang -> return lang
            _ -> ioError $ userError $ (x:y:ys) ++ " is wrong LC_CTYPE value"
        _ -> ioError $ userError "locale is not set"

main :: IO ()
main = do
    lang <- catchIOError language (\_ -> return EN)
    run 80 (app lang)
