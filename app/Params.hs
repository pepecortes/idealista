{-# LANGUAGE OverloadedStrings #-}

module Params where

import Options.Applicative
import Data.Text ( Text )
import Data.ByteString (ByteString)
import System.Directory ( getHomeDirectory )
import Data.Configurator as C
    ( load, lookupDefault, Worth(Required) )

data AppConfig = AppConfig {
    apiKey :: Text
  , apiHost :: Text
  , authPath :: Text
  , searchPath :: Text
  , searchParams :: [(ByteString, ByteString)]
  , databasePath :: FilePath
} deriving ( Show )

data Subcommand = 
    Update
  | DisplayAvailable
  | DisplayHistory
  | DisplaySingle Text
  | Test
  deriving ( Show )

data Params = Params {
    configFile :: FilePath
  , subcommand :: Subcommand
  } deriving ( Show )

loadConfig :: FilePath -> IO AppConfig
loadConfig filePath = do
  conf <- load [Required filePath]
  AppConfig
    <$> lookupDefault "" conf "apiKey"
    <*> lookupDefault "" conf "apiHost"
    <*> lookupDefault "" conf "authPath"
    <*> lookupDefault "" conf "searchPath"
    <*> lookupDefault [] conf "searchParams"
    <*> lookupDefault "" conf "databaseFilePath"

inputIO :: IO (Parser Params)
inputIO = do
  configFileParser <- configFileParserIO
  let rawParser = Params <$> configFileParser <*> subcommandParser
  return $ rawParser <**> helper

configFileParserIO :: IO (Parser FilePath)
configFileParserIO = do
  home <- getHomeDirectory
  return $ strOption (
       short 'c'
    <> long "config" 
    <> help "Alternate configuration file (default: $HOME/.idealista/config)" 
    <> value (home ++ "/.idealista/config")
    <> metavar "FILE_PATH"
    )

subcommandParser :: Parser Subcommand
subcommandParser = subparser $
      command "update" (info (pure Update) (progDesc "Download updated data and update the archive"))
   <> command "avail" (info (pure DisplayAvailable) ( progDesc "Display only available pisos" ))
   <> command "history" (info (pure DisplayHistory) ( progDesc "Display historical data" ))
   <> command "display" (info displaySingle ( progDesc "Display piso given by its property code" ))
   <> command "test" (info (pure Test) ( progDesc "Run system tests" ))

displaySingle :: Parser Subcommand
displaySingle = DisplaySingle <$> strArgument
  (  metavar "PROPERTY_CODE"
  <> help "Data of the given property code" )

cmdLineParser :: IO Params
cmdLineParser = do
    parser <- inputIO
    let modifiers = fullDesc <> progDesc "Idealista.com HTML data processing"
    execParser $ info parser modifiers