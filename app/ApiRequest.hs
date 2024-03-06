{-# LANGUAGE OverloadedStrings #-}

module ApiRequest where

import Params

import Data.Text ( Text )
import qualified Data.Text as T (append)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import Data.Aeson ( Value(String, Object) )
import qualified Data.Aeson.KeyMap as K (lookup)
import Data.ByteString (ByteString)
import Control.Monad.Reader ( ReaderT, MonadIO(liftIO), asks )
import Data.Aeson.Types (Key)
import Data.Aeson.Key (toText)


basicRequest :: ReaderT AppConfig IO Request
basicRequest = do
  host <- asks apiHost
  let request = setRequestMethod "POST"
              $ setRequestPort 443
              $ setRequestSecure True
              $ setRequestHost (encodeUtf8 host)
              defaultRequest
  pure request


authHeaders :: Text -> RequestHeaders
authHeaders key = [
      ("Authorization", encodeUtf8 $ T.append "Basic " key)
    , ("Content-Type", "application/x-www-form-urlencoded")
    ]

authParams :: [(ByteString, ByteString)]
authParams = [
      ("grant_type", "client_credentials")
    , ("scope", "read")
    ]

searchHeaders :: Text -> RequestHeaders
searchHeaders token = [
      ("Authorization", encodeUtf8 $ T.append "Bearer " token )
    , ("Content-Type", "application/x-www-form-urlencoded")
    ]

extractJson :: Key -> Int -> Value -> Either Value Value
extractJson key 200 (Object json) = get (K.lookup key json)
  where
    get Nothing = Left $ String (T.append "key not found: " (toText key))
    get (Just value) = Right value
extractJson _   200 value = Left value
extractJson _   _   value = Left value

extractString :: Key -> Int -> Value -> Either Value Text
extractString key status body = get $ extractJson key status body
  where 
    get (Left value) = Left value
    get (Right (String str)) = Right str
    get (Right _) = Left $ String "expected string, but not found"
    

-- | Return the headers needed to carry out an API call,
-- in particular, request the authorization token and put it in the headers
-- Also: set content-type to x-www-form-urlencoded
apiCallHeaders :: ReaderT AppConfig IO RequestHeaders
apiCallHeaders = do
  path <- asks authPath
  key <- asks apiKey
  req <- basicRequest
  liftIO $ do
    let request
            = setRequestPath (encodeUtf8 path)
            $ setRequestHeaders (authHeaders key)
            $ setRequestBodyURLEncoded authParams
            req
    response <- httpJSON request
    let status = getResponseStatusCode response
    let body = getResponseBody response :: Value
    let token = either 
                  (error "authorization error")
                  id 
                  $ extractString "access_token" status body
    pure [
           ("Authorization", encodeUtf8 $ T.append "Bearer " token )
         , ("Content-Type", "application/x-www-form-urlencoded")
         ]

-- | Return an authorized request: a request for which we have requested the
-- authorization headers
authorizedRequest :: ReaderT AppConfig IO Request
authorizedRequest = do
  headers <- apiCallHeaders
  setRequestHeaders headers <$> basicRequest


-- | Return a search based on the query defined in config
requestSearch :: ReaderT AppConfig IO (Either Value Value)
requestSearch = do
  path <- asks searchPath
  query <- asks searchParams
  req <- authorizedRequest
  liftIO $ do
    let request 
          = setRequestPath (encodeUtf8 path)
          $ setRequestBodyURLEncoded query
          req
    response <- httpJSON request
    let status = getResponseStatusCode response
    let value = getResponseBody response :: Value
    pure $ extractJson "elementList" status value

