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
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.CaseInsensitive (CI)
import qualified Data.HashMap.Lazy as LH
import Data.LanguageCodes (ISO639_1)
import Data.Set (Set, fromList, notMember)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, rawPathInfo, rawQueryString,
                    responseLBS, requestBody, requestHeaders, requestMethod)
import Network.Wreq (Options, checkResponse, defaults, deleteWith, getWith,
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

data ProxyAction = Pass | Translate ContentType deriving (Eq, Show)
data ContentType = Json | Xml deriving (Eq, Show)

proxyApp :: ProxyConfiguration -> Text -> Application
proxyApp config url req respond = do
    body <- requestBody req
    response <- request method options (unpack url) body
    let rBody = response ^. responseBody
        contentType = response ^. responseHeader "Content-Type"
        (mimetype, _) = B.breakByte 0x3b contentType  -- drop after semicolon
        rStatus = (response ^. responseStatus)
        rHeaders = (response ^. responseHeaders)
        proxyAction = case (method, mimetype) of
            ("POST", "text/xml") -> Translate Xml
            ("POST", "text/json") -> Translate Json
            ("POST", "application/json") -> Translate Json
            _ -> Pass
        f = case proxyAction of
            Pass -> return
            Translate t -> translateResponse (apiKey config) (language config) t
    translated <- f rBody
    let headers = [ (name, value)
                  | (name, value) <- rHeaders
                  , name /= "content-length" && name `notMember` hoppishHeaders
                  ]
        -- Content-Length becomes invalid since the translated text doesn't
        -- have the same length to its source text
    respond $ responseLBS rStatus headers translated
  where
    method = requestMethod req
    headerList = requestHeaders req
    acceptAnyStatus _ _ = return ()
    options = defaults & headers .~ [(k, v) | (k, v) <- headerList
                                            , k `notMember` hoppishHeaders]
                       & checkResponse .~ (Just acceptAnyStatus)

request :: Method
        -> Options -> String -> B.ByteString -> IO (Response LB.ByteString)
request "GET" = \options url _ -> getWith options url
request "POST" = postWith
request "PUT" = putWith
request "DELETE" = \options url _ -> deleteWith options url
request _ = \_ _ _ -> ioError $ userError $ "unsupported method"

translateResponse :: ApiKey
                  -> ISO639_1
                  -> ContentType
                  -> LB.ByteString
                  -> IO LB.ByteString
translateResponse apiKey' lang Xml response =
    case parseLBS def response of
        Left _ -> return response
        Right doc -> do
            translatedDoc <- translateXml apiKey' lang doc
            return $ renderLBS def translatedDoc
translateResponse apiKey' lang Json response =
    case decoded of
        Nothing -> return response
        Just array -> do
            translated <- translateJson apiKey' lang array
            return $ A.encode translated
  where
    decoded :: Maybe [A.Object]
    decoded = A.decode response

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

translateJson :: ApiKey -> ISO639_1 -> [A.Object] -> IO [A.Object]
translateJson apiKey' lang array = do
    translatedTexts <- translate apiKey' lang texts
    let index = LH.fromList $ zip texts translatedTexts
    return [ case t of
                 Nothing -> o
                 Just t' -> case LH.lookup t' index of
                     Just translated -> updateChatContent o translated
           | (o, t) <- pairs
           ]
  where
    pairs :: [(A.Object, Maybe Text)]
    pairs = [ (o, chatContent o) | o <- array ]
    texts :: [Text]
    texts = [text | (_, Just text) <- pairs ]
    chatContent :: A.Object -> Maybe Text
    chatContent o = do
        chat' <- LH.lookup "chat" o
        chat <- case chat' of
            A.Object c -> return c
            _ -> Nothing
        content <- LH.lookup "content" chat
        case content of
            A.String t -> return t
            _ -> Nothing
    adjustH k h f = LH.adjust f k h
    updateChatContent :: A.Object -> Text -> A.Object
    updateChatContent o text =
        adjustH "chat" o $ \chat' ->
            case chat' of
                A.Object chat -> A.Object $ adjustH "content" chat $ \c ->
                    case c of
                        A.String _ -> A.String text
                        _ -> c
                _ -> chat'
