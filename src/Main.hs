{-# LANGUAGE OverloadedStrings #-}

module Main where

import Params
import JsonWorker ( loadNewData )
import PisoReport ( reportFromId, textReport )
import PisoData ( PisoData(propertyCode) )
import Archiver ( loadArchive, updateArchive )
import ApiRequest ()

import Data.Text ( Text )
import qualified Data.Text.IO as TIO ( putStrLn )
import Control.Monad.Reader
    ( ReaderT(runReaderT), MonadIO(liftIO) )
import Data.Configurator as C
    ( load, lookupDefault, Worth(Required) )


workUpdate :: ReaderT AppConfig IO ()
workUpdate = do
  Just newPisos <- loadNewData
  pisos <- updateArchive newPisos
  liftIO $ do
    putStr "Pisos data archive updated: "
    print (length pisos)
    return ()

workHistorical :: ReaderT AppConfig IO ()
workHistorical = do
  pisos <- loadArchive
  liftIO $ putStrLn (textReport pisos)

workAvailable :: ReaderT AppConfig IO ()
workAvailable = do
  Just newPisos <- loadNewData
  pisos <- updateArchive newPisos
  liftIO $ do
    let availablePisos = filter (\p -> propertyCode p `elem` map propertyCode newPisos) pisos
    putStrLn $ textReport availablePisos

workSinglePiso :: Text -> ReaderT AppConfig IO ()
workSinglePiso code = do
  Just newPisos <- loadNewData
  pisos <- updateArchive newPisos
  liftIO $ TIO.putStrLn $ reportFromId pisos code

work :: Params -> ReaderT AppConfig IO ()
work Params{subcommand=Update} = workUpdate
work Params{subcommand=DisplayAvailable} = workAvailable
work Params{subcommand=DisplayHistory} = workHistorical
work Params{subcommand=DisplaySingle str} = workSinglePiso str

loadConfig :: FilePath -> IO AppConfig
loadConfig filePath = do
  conf <- load [Required filePath]
  AppConfig 
    <$> lookupDefault "" conf "apiKey"
    <*> lookupDefault "" conf "authURL"
    <*> lookupDefault "" conf "searchURL" 
    <*> lookupDefault [] conf "searchParams"
    <*> lookupDefault "" conf "archiveFilePath" 
    
main :: IO ()
main = do
  params <- cmdLineParser
  config <- loadConfig $ configFile params
  _ <- runReaderT (work params) config
  return ()

