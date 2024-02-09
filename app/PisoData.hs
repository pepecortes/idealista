{-# LANGUAGE DeriveGeneric #-}

module PisoData where

import Data.Text ( Text )
import GHC.Generics ( Generic )
import Data.Aeson
    ( defaultOptions, genericToEncoding, FromJSON, ToJSON(toEncoding) )

data PisoData = PisoData {
    propertyCode ::  Text
  , url :: Text
  , exterior :: Bool
  , floor ::  Text
  , price :: Integer
  , rooms :: Integer
  , size :: Integer
  , status :: String
  , hasLift :: Bool
  , description :: Text
} deriving (Generic, Show)

instance ToJSON PisoData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PisoData

same :: PisoData -> PisoData -> Bool
same PisoData{propertyCode = id1} PisoData{propertyCode=id2} = id1 == id2