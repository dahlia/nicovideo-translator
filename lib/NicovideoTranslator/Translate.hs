{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module NicovideoTranslator.Translate (ApiKey, translate) where

import GHC.Exts (IsList (toList))
import Prelude hiding (lookup)

import Control.Concurrent.Async (concurrently)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson.Types (Value (Array, Bool, Object, String), toJSON)
import Data.HashMap.Strict (lookup)
import Data.LanguageCodes (ISO639_1)
import Data.Text (Text, pack, toLower, unpack)
import Network.Wreq ( Options
                    , Response
                    , asJSON
                    , defaults
                    , param
                    , postWith
                    , responseBody
                    )

type ApiKey = Text

apiUrl :: String
apiUrl = "https://translation.googleapis.com/language/translate/v2"

translate :: ApiKey -> ISO639_1 -> [Text] -> IO [Text]
translate apiKey target texts =
    case splitAt 128 texts of
        ([], _) -> return []
        (head, []) -> translate' apiKey target head
        (head, tail) -> do
            let trans = translate apiKey target
            (headResult, tailResult) <- concurrently (trans head) (trans tail)
            return $ headResult ++ tailResult

translate' :: ApiKey -> ISO639_1 -> [Text] -> IO [Text]
translate' apiKey target texts = do
    response <- (asJSON =<< postWith query apiUrl params) :: IO (Response Value)
    let Object body = response ^. responseBody
        Just (Object data') = lookup "data" body
        Just (Array translations) = lookup "translations" data'
    return $ [ s
             | Object r <- toList translations
             , Just (String s) <- [lookup "translatedText" r]
             ]
  where
    query :: Options
    query = defaults & param "key" .~ [apiKey]
    params :: Value
    params = Object
        [ ("target", String $ toLower . pack . show $ target)
        , ("source", String "ja")
        , ("prettyprint", Bool False)
        , ("format", String "text")
        , ("q", toJSON texts)
        ]
