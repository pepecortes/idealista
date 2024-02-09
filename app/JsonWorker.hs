{-# LANGUAGE ScopedTypeVariables #-}

module JsonWorker (loadNewData) where

import Params ( AppConfig(searchParams) )
import PisoData ( PisoData )
import ApiRequest ( requestAuth, requestSearch )

import Data.Aeson ( FromJSON(parseJSONList) )
import Data.Aeson.Types ( parseMaybe )
import Control.Monad.Reader ( ReaderT, asks )

loadNewData :: ReaderT AppConfig IO (Maybe [PisoData])
loadNewData = do
    token <- requestAuth
    queryParams <- asks searchParams
    value <- requestSearch token queryParams
    pure $ parseMaybe parseJSONList value