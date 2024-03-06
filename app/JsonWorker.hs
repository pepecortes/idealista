{-# LANGUAGE ScopedTypeVariables #-}

module JsonWorker (loadNewData) where

import Params ( AppConfig )
import PisoData ( PisoData )
import ApiRequest ( requestSearch )

import Data.Aeson ( FromJSON(parseJSONList) )
import Data.Aeson.Types ( parseMaybe )
import Control.Monad.Reader ( ReaderT )

loadNewData :: ReaderT AppConfig IO (Maybe [PisoData])
loadNewData = do
  response <- requestSearch
  let pisos = either
                (error "API error: unable to load new data")
                (parseMaybe parseJSONList)
                response
  pure pisos