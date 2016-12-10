{-# LANGUAGE OverloadedStrings #-}
module NicovideoTranslator.Proxy
    ( ProxyConfiguration ( ProxyConfiguration
                         , apiKey
                         , language
                         , upstreamHost
                         )
    , app
    ) where

import Data.List (find)
import Data.Maybe (catMaybes)

import Control.Lens ((&), (.~), (^.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.CaseInsensitive (CI)
import Data.LanguageCodes (ISO639_1)
import Data.Set (Set, fromList, notMember)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, rawPathInfo, rawQueryString,
                    responseLBS, requestBody, requestHeaders, requestMethod)
import Network.Wreq (Options, checkStatus, defaults, deleteWith, getWith,
                     headers, postWith, putWith, responseBody,
                     responseHeader, responseHeaders, responseStatus)
import Network.Wreq.Lens (Response)
import Text.XML (Document(Document), Element(Element),
                 Node(NodeContent, NodeElement),
                 def, elementNodes, parseLBS, renderLBS)
import Text.XML.Cursor (content, element, fromDocument, node, ($//), (&//))

import NicovideoTranslator.Translate (ApiKey, translate)

data ProxyConfiguration =
    ProxyConfiguration { language :: ISO639_1
                       , upstreamHost :: Text
                       , apiKey :: ApiKey
                       }

app :: ProxyConfiguration -> Application
app config req respond =
    let path = rawPathInfo req
        qs = rawQueryString req
        host = encodeUtf8 $ upstreamHost config
        pathQuery = B.append path qs
        hostUrl = B.append "http://" host
        url = B.append hostUrl pathQuery
        urlString = decodeUtf8 url
    in proxyApp config urlString req respond

hoppishHeaders :: Set (CI B.ByteString)
hoppishHeaders = fromList [ "connection"
                          , "content-encoding"
                          , "keep-alive"
                          , "proxy-authenticate"
                          , "proxy-authorization"
                          , "te"
                          , "trailers"
                          , "transfer-encoding"
                          , "upgrade"
                          , "content-encoding"
                          ]

proxyApp :: ProxyConfiguration -> Text -> Application
proxyApp config url req respond = do
    body <- requestBody req
    response <- request method options (unpack url) body
    let rBody = response ^. responseBody
        contentType = response ^. responseHeader "Content-Type"
        (mimetype, _) = B.breakByte 0x3b contentType  -- drop after semicolon
        rStatus = (response ^. responseStatus)
        rHeaders = (response ^. responseHeaders)
        toBeTranslated = method == "POST" && mimetype == "text/xml"
    translated <- if toBeTranslated
                  then translateResponse (apiKey config)
                                         (language config)
                                         rBody
                  else return rBody
    let headers = [ (name, value)
                  | (name, value) <- rHeaders
                  , name /= "content-length"
                  ]
        -- Content-Length becomes invalid since the translated text doesn't
        -- have the same length to its source text
    respond $ responseLBS rStatus headers translated
  where
    method = requestMethod req
    headerList = requestHeaders req
    acceptAnyStatus _ _ _ = Nothing
    options = defaults & headers .~ [(k, v) | (k, v) <- headerList
                                            , k `notMember` hoppishHeaders]
                       & checkStatus .~ (Just acceptAnyStatus)

request :: Method
        -> Options -> String -> B.ByteString -> IO (Response LB.ByteString)
request "GET" = \options url _ -> getWith options url
request "POST" = postWith
request "PUT" = putWith
request "DELETE" = \options url _ -> deleteWith options url
request _ = \_ _ _ -> ioError $ userError $ "unsupported method"

translateResponse :: ApiKey -> ISO639_1 -> LB.ByteString -> IO LB.ByteString
translateResponse apiKey' lang response = case parseLBS def response of
    Left _ -> return response
    Right doc -> do translatedDoc <- translateXml apiKey' lang doc
                    return $ renderLBS def translatedDoc

translateXml :: ApiKey -> ISO639_1 -> Document -> IO Document
translateXml apiKey' lang doc = do
    translatedTexts <- translate apiKey' lang texts
    let translatedElems = [ (el, el { elementNodes = [NodeContent text] })
                          | (el, text) <- zip elems translatedTexts ]
    return $ transformXml doc translatedElems
  where
    cursor = fromDocument doc
    texts :: [Text]
    texts = cursor $// element "chat" &// content
    elems :: [Element]
    elems = catMaybes [ case node node' of
                             NodeElement element' -> Just element'
                             _ -> Nothing
                      | node' <- cursor $// element "chat" ]

transformXml :: Document -> [(Element, Element)] -> Document
transformXml (Document prolog root epilog) table =
    Document prolog (transformElement root table) epilog

transformElement :: Element -> [(Element, Element)] -> Element
transformElement el table =
    case find (\(src, _) -> src == el) table of
        Just (_, dst) -> dst
        Nothing -> Element name attrs [ case node' of
                                            NodeElement e -> NodeElement $
                                                transformElement e table
                                            _ -> node'
                                      | node' <- nodes ]
  where
    Element name attrs nodes = el
