{-# LANGUAGE OverloadedStrings #-}
module NicovideoTranslator.Proxy (app) where

import Data.List (find)
import Data.Maybe (catMaybes)

import Control.Lens ((&), (.~), (^.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.CaseInsensitive (CI)
import Data.LanguageCodes (ISO639_1)
import Data.Set (Set, fromList, notMember)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, rawPathInfo, rawQueryString,
                    responseLBS, requestBody, requestHeaders, requestMethod,
                    requestHeaderHost)
import Network.Wreq (Options, checkStatus, defaults, deleteWith, getWith,
                     headers, postWith, putWith, responseBody,
                     responseHeader, responseHeaders, responseStatus)
import Network.Wreq.Lens (Response)
import Text.XML (Document(Document), Element(Element),
                 Node(NodeContent, NodeElement),
                 def, elementNodes, parseLBS, renderLBS)
import Text.XML.Cursor (content, element, fromDocument, node, ($//), (&//))

import NicovideoTranslator.Translate (translateMultiple)

app :: ISO639_1 -> Application
app language req respond =
    let path = rawPathInfo req
        qs = rawQueryString req
        host = case requestHeaderHost req of
                   Just hostname -> hostname
                   Nothing -> defaultHost
        pathQuery = B.append path qs
        hostUrl = B.append "http://" host
        url = B.append hostUrl pathQuery
        urlString = decodeUtf8 url
    in proxyApp language urlString req respond

defaultHost :: B.ByteString
defaultHost = "msg.nicovideo.jp"

hoppishHeaders :: Set (CI B.ByteString)
hoppishHeaders = fromList [ "connection"
                          , "keep-alive"
                          , "proxy-authenticate"
                          , "proxy-authorization"
                          , "te"
                          , "trailers"
                          , "transfer-encoding"
                          , "upgrade"
                          , "content-encoding"
                          ]

proxyApp :: ISO639_1 -> Text -> Application
proxyApp language url req respond = do
    body <- requestBody req
    response <- request method options (unpack url) body
    let rBody = response ^. responseBody
        contentType = response ^. responseHeader "Content-Type"
        (mimetype, _) = B.breakByte 0x3b contentType  -- drop after semicolon
        rStatus = (response ^. responseStatus)
        rHeaders = (response ^. responseHeaders)
        toBeTranslated = method == "POST" && mimetype == "text/xml"
    translated <- if toBeTranslated
                  then translateResponse language rBody
                  else return rBody
    respond $ responseLBS rStatus rHeaders translated
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
request _= \_ _ _ -> ioError $ userError $ "unsupported method"

translateResponse :: ISO639_1 -> LB.ByteString -> IO LB.ByteString
translateResponse language response = case parseLBS def response of
    Left _ -> return response
    Right doc -> do translatedDoc <- translateXml language doc
                    return $ renderLBS def translatedDoc

translateXml :: ISO639_1 -> Document -> IO Document
translateXml language doc = do
    translatedTexts <- translateMultiple language texts
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
