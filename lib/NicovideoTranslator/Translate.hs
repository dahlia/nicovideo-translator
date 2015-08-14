{-# LANGUAGE OverloadedStrings #-}
module NicovideoTranslator.Translate
    (
      translate
    , translateMultiple
    ) where

import qualified Language.Translate.Naver as Naver
import Data.LanguageCodes (ISO639_1(JA))
import Data.Text (Text)

translate :: ISO639_1 -> Text -> IO Text
translate = Naver.translate JA

translateMultiple :: ISO639_1 -> [Text] -> IO [Text]
translateMultiple = Naver.translateMultiple JA
