module Params where

import Options.Applicative
import Data.Text ( Text )
import Data.ByteString (ByteString)
import System.Directory ( getHomeDirectory )

data AppConfig = AppConfig {
    apiKey :: Text
  , authURL :: Text
  , searchURL :: Text
  , searchParams :: [(ByteString, ByteString)]
  , archivePath :: FilePath
} deriving ( Show )

data Subcommand = 
    Update
  | DisplayAvailable
  | DisplayHistory
  | DisplaySingle Text
  deriving ( Show )

data Params = Params {
    configFile :: FilePath
  , subcommand :: Subcommand
  } deriving ( Show )

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

displaySingle :: Parser Subcommand
displaySingle = DisplaySingle <$> strArgument
  (  metavar "PROPERTY_CODE"
  <> help "Data of the given property code" )

cmdLineParser :: IO Params
cmdLineParser = do
    parser <- inputIO
    let modifiers = fullDesc <> progDesc "Idealista.com HTML data processing"
    execParser $ info parser modifiers