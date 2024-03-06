module Main (main) where
  
import Params
    ( cmdLineParser,
      loadConfig,
      AppConfig,
      Params(..),
      Subcommand(Test, Update, DisplayAvailable, DisplayHistory,
                 DisplaySingle) )
import JsonWorker ( loadNewData )
import PisoReport ( reportFromId, textReport )
import Archiver ( updateArchive, loadArchiveDB )

import Data.Text ( Text )
import qualified Data.Text.IO as TIO ( putStrLn )
import Control.Monad.Reader
    ( ReaderT(runReaderT), MonadIO(liftIO) )

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
  pisos <- loadArchiveDB
  liftIO $ putStrLn (textReport pisos)

workAvailable :: ReaderT AppConfig IO ()
workAvailable = do
  Just newPisos <- loadNewData
  pisos <- updateArchive newPisos
  liftIO $ putStrLn $ textReport pisos

workSinglePiso :: Text -> ReaderT AppConfig IO ()
workSinglePiso code = do
  Just newPisos <- loadNewData
  pisos <- updateArchive newPisos
  liftIO $ TIO.putStrLn $ reportFromId pisos code

workTest :: ReaderT AppConfig IO ()
workTest = do
  Just newPisos <- loadNewData
  liftIO $ print newPisos

work :: Params -> ReaderT AppConfig IO ()
work Params{subcommand=Update} = workUpdate
work Params{subcommand=DisplayAvailable} = workAvailable
work Params{subcommand=DisplayHistory} = workHistorical
work Params{subcommand=DisplaySingle str} = workSinglePiso str
work Params{subcommand=Test} = workTest

main :: IO ()
main = do
  params <- cmdLineParser
  config <- loadConfig $ configFile params
  _ <- runReaderT (work params) config
  return ()

