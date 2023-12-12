{-# LANGUAGE OverloadedStrings #-}

module PisoReport where

import PisoData

import Colonnade (ascii, headed)
import Fmt ( (+|), fmt, (|+), indentF, nameF )
import Data.List (find)
import Data.Time ( UTCTime, formatTime, defaultTimeLocale )
import Data.Text ( Text )
import qualified Data.Text as T (append)


textReport :: [PisoData] -> String
textReport = ascii colStats
    where
        colStats = mconcat [
              headed "fecha" (formatDate . firstSeen)
            , headed "url" (noQuotes . show . url)
            , headed "m2" (show . size)
            , headed "piso" pisoAscensor
            , headed "habitaciones" (show . rooms)
            , headed "ext/int" (extInt . exterior)
            , headed "estado" (noQuotes . show . status)
            , headed "precio" (show . price)
            , headed "visto en" (formatDate . recentSeen)
            , headed "ultimo precio" (showMaybe . lastPrice)
            ]

showMaybe :: Show a => Maybe a -> String
showMaybe Nothing = "-"
showMaybe (Just v) = show v

noQuotes :: String -> String
noQuotes = filter (/= '"')

formatDate :: Maybe UTCTime -> String
formatDate Nothing = "-"
formatDate (Just date)= formatTime defaultTimeLocale "%d-%b-%Y" date

extInt :: Bool -> String
extInt True = "exterior"
extInt False = "interior"

pisoAscensor :: PisoData -> String
pisoAscensor pd | hasLift pd = noQuotes $ show piso
                | otherwise  = noQuotes $ show $ T.append piso " (sin ascensor)"
  where
    piso = PisoData.floor pd

reportFromId :: [PisoData] -> Text -> Text
reportFromId pd id = singlePisoReport piso
  where piso = find (\piso -> propertyCode piso == id) pd

singlePisoReport :: Maybe PisoData -> Text
singlePisoReport Nothing = "Not found"
singlePisoReport (Just piso) = fmt
  $  "\n"+|url piso|+"\n\n"
  <> indentF 4 datos
  <> "\n"+|description piso|+""
  where
    datos = fmt
      $  nameF "precio" (""+|price piso|+" ("+|formatDate (firstSeen piso)|+")\n")
      <> nameF "ultimo precio" (""+|lastPrice piso|+" ("+|formatDate (recentSeen piso)|+")\n")
      <> nameF "piso" (""+|pisoAscensor piso|+"\n")
      <> nameF "superficie" (""+|size piso|+" m2\n")
      <> nameF "exterior" (""+|exterior piso|+"\n")
      <> nameF "habitaciones" (""+|rooms piso|+"\n")
      <> nameF "estado" (""+|status piso|+"\n")
