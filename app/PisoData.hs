{-# LANGUAGE DeriveGeneric #-}

module PisoData where

import Data.Text ( Text )
import GHC.Generics ( Generic )
import Data.Time 
    ( defaultTimeLocale, formatTime, getCurrentTime, UTCTime )
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
  , firstSeen :: Maybe UTCTime
  , recentSeen :: Maybe UTCTime
  , lastPrice :: Maybe Integer
} deriving (Generic, Show)

instance ToJSON PisoData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PisoData

same :: PisoData -> PisoData -> Bool
same PisoData{propertyCode = id1} PisoData{propertyCode=id2} = id1 == id2

timeStamp :: PisoData -> IO PisoData
timeStamp piso = do
  now <- getCurrentTime
  let out = checkStamp piso now
  return out
    where
      checkStamp p@PisoData{ firstSeen = Nothing } date = p{ firstSeen = Just date }
      checkStamp p@PisoData{ firstSeen = Just _ } date = p{ recentSeen = Just date }
