{-# LANGUAGE OverloadedStrings #-}

module ApiRequest (requestAuth, requestSearch) where

import Params ( AppConfig(searchURL, authURL, apiKey) )

import Data.Text ( Text, unpack )
import qualified Data.Text as T (append)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
    ( RequestHeaders,
      parseRequest,
      getResponseBody,
      getResponseStatusCode,
      httpJSON,
      setRequestBodyURLEncoded,
      setRequestHeaders,
      setRequestMethod )
import Data.Aeson ( Value(String, Object) )
import qualified Data.Aeson.KeyMap as K (lookup)
import Data.ByteString (ByteString)
import Control.Monad ( when )
import Control.Monad.Reader ( ReaderT, MonadIO(liftIO), asks )

authHeaders :: Text -> RequestHeaders
authHeaders apiKey = [
      ("Authorization", encodeUtf8 $ T.append "Basic " apiKey)
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

requestAuth :: ReaderT AppConfig IO Text
requestAuth = do
  authURL <- asks authURL
  apiKey <- asks apiKey
  liftIO $ do
    request' <- parseRequest (unpack authURL)
    let request
            = setRequestMethod "POST"
            $ setRequestHeaders (authHeaders apiKey)
            $ setRequestBodyURLEncoded authParams request'
    response <- httpJSON request
    let (Object json) = getResponseBody response :: Value
    let Just (String token) = K.lookup "access_token" json
    return token

requestSearch :: Text -> [(ByteString, ByteString)] -> ReaderT AppConfig IO Value
requestSearch token queryParams = do
  searchURL <- asks searchURL
  liftIO $ do
    request' <- parseRequest (unpack searchURL)
    let request
            = setRequestMethod "POST"
            $ setRequestHeaders (searchHeaders token)
            $ setRequestBodyURLEncoded queryParams request'
    response <- httpJSON request
    let status = getResponseStatusCode response
    let (Object json) = getResponseBody response :: Value
    when (status /= 200) (print json)
    let Just v = K.lookup "elementList" json
    return v

