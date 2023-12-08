{-# LANGUAGE ScopedTypeVariables #-}

module JsonWorker where

import Params ( AppConfig(searchParams) )
import PisoData ( PisoData(price, lastPrice) )
import ApiRequest ( requestAuth, requestSearch )

import Data.Aeson ( decode, encode, FromJSON(parseJSONList) )
import Data.Aeson.Types ( parseMaybe )
import Control.Monad.Reader ( ReaderT, MonadIO(liftIO), asks )

loadNewData :: ReaderT AppConfig IO (Maybe [PisoData])
loadNewData = do
    token <- requestAuth
    queryParams <- asks searchParams
    value <- requestSearch token queryParams
    liftIO $ do
        let rawPisos = parseMaybe parseJSONList value
        let pisos = map (\p ->  p{ lastPrice = Just $ price p }) <$> rawPisos
        return pisos