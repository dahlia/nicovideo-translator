module Main (main) where

import Data.Char (toLower)
import Data.LanguageCodes (ISO639_1(EN), fromChars)
import Data.Text (pack, unpack)
import NicovideoTranslator (translate)
import System.IO.Error (catchIOError)
import System.Locale.SetLocale (Category(LC_CTYPE), setLocale)

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
    contents <- getContents
    translated <- translate lang $ pack contents
    putStr $ unpack translated
