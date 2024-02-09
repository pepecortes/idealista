{-# LANGUAGE OverloadedStrings #-}

module PisoReport where

import Schemas

import Colonnade (ascii, headed)
import Fmt ( (+|), fmt, (|+), indentF, nameF )
import Data.List (find)
import Data.Time ( UTCTime, formatTime, defaultTimeLocale )
import Data.Text ( Text )
import qualified Data.Text as T (append)


textReport :: [Piso] -> String
textReport = ascii colStats
    where
        colStats = mconcat [
              headed "fecha" (formatDate . pisoFirstSeen)
            , headed "url" (noQuotes . show . pisoUrl)
            , headed "m2" (show . pisoSize)
            , headed "piso" pisoAscensor
            , headed "habitaciones" (show . pisoRooms)
            , headed "ext/int" (extInt . pisoExterior)
            , headed "estado" (noQuotes . show . pisoStatus)
            , headed "precio" (show . pisoPrice)
            , headed "visto en" (formatDate . pisoRecentSeen)
            , headed "ultimo precio" (showMaybe . pisoLastPrice)
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

pisoAscensor :: Piso -> String
pisoAscensor pd | pisoHasLift pd = noQuotes $ show piso
                | otherwise  = noQuotes $ show $ T.append piso " (sin ascensor)"
  where
    piso = pisoFloor pd

reportFromId :: [Piso] -> Text -> Text
reportFromId pd id = singlePisoReport piso
  where piso = find (\piso -> pisoPropertyCode piso == id) pd

singlePisoReport :: Maybe Piso -> Text
singlePisoReport Nothing = "Not found"
singlePisoReport (Just piso) = fmt
  $  "\n"+|pisoUrl piso|+"\n\n"
  <> indentF 4 datos
  <> "\n"+|pisoDescription piso|+""
  where
    datos = fmt
      $  nameF "precio" (""+|pisoPrice piso|+" ("+|formatDate (pisoFirstSeen piso)|+")\n")
      <> nameF "ultimo precio" (""+|pisoLastPrice piso|+" ("+|formatDate (pisoRecentSeen piso)|+")\n")
      <> nameF "piso" (""+|pisoAscensor piso|+"\n")
      <> nameF "superficie" (""+|pisoSize piso|+" m2\n")
      <> nameF "exterior" (""+|pisoExterior piso|+"\n")
      <> nameF "habitaciones" (""+|pisoRooms piso|+"\n")
      <> nameF "estado" (""+|pisoStatus piso|+"\n")
