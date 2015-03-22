{-# LANGUAGE OverloadedStrings #-}
module NicovideoTranslator (translate, translateMultiple) where

import Control.Lens ((^?))
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Value(String))
import Data.ByteString.Lazy.Internal (ByteString)
import Data.LanguageCodes (ISO639_1, language)
import Data.Text (Text, cons, intercalate, pack, snoc, splitOn, strip)
import Network.Wreq (Response, FormParam((:=)), post, responseBody)
import System.Random (getStdRandom, randomR)

translate :: ISO639_1 -> Text -> IO Text
translate lang text =
    let response =
            post
            "http://translate.naver.com/translate.dic"
            [
                "query" := text,
                "srcLang" := ("ja" :: Text),
                "tarLang" := (pack $ language lang),
                "highlight" := ("0" :: Text),
                "hurigana" := ("0" :: Text)
            ]
        getResultData response = response ^? responseBody . key "resultData"
        resultData = response >>= return . getResultData
    in
        resultData >>= \dat -> case dat of
            Just (String translated) -> return translated
            _ -> ioError $ userError "translate.naver.com sent invalid response"

arbitraryNumeric :: IO Text
arbitraryNumeric =
    let
        randomDigit i = getStdRandom (randomR (if i > 1 then '0' else '1', '9'))
        string = sequence $ map randomDigit [1..20]
    in
        string >>= return . pack

translateMultiple :: ISO639_1 -> [Text] -> IO [Text]
translateMultiple lang texts = do
    spliter <- arbitraryNumeric
    result <- translate lang $ intercalate (snoc (cons ' ' spliter) ' ') texts
    return $ map strip $ splitOn spliter result
