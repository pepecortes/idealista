module Archiver where

import Params ( AppConfig(archivePath) )
import PisoData ( timeStamp, PisoData(propertyCode) )

import Data.Aeson
    ( FromJSON(parseJSONList),
      ToJSON(toEncoding),
      encode,
      decode,
      defaultOptions,
      genericToEncoding, encodeFile )
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Monad ( foldM )
import Control.Monad.Reader
    ( ReaderT, foldM, MonadIO(liftIO), asks )

firstArchive :: [PisoData] -> ReaderT AppConfig IO ()
firstArchive pd = do
  path <- asks archivePath
  liftIO $ do
    let str = encode pd
    BL8.writeFile path str

updateArchive :: [PisoData] -> ReaderT AppConfig IO [PisoData]
updateArchive pisos = do
  archive <- loadArchive
  path <- asks archivePath
  liftIO $ do
    archive' <- checkAllPisos archive pisos
    encodeFile path archive'
    return archive'

loadArchive :: ReaderT AppConfig IO [PisoData]
loadArchive = do
  path <- asks archivePath
  liftIO $ do
    str <- BL8.readFile path
    let (Just pisos) = decode str
    return pisos

-- Check if the given piso is already in the list
-- If it is the case, update the recentSeen attribute
-- If not, timestamp it and add it to the list
checkNewPiso :: [PisoData] -> PisoData -> IO [PisoData]
checkNewPiso [] e = do e' <- timeStamp e
                       return [e']
checkNewPiso (p:ps) e | propertyCode p == propertyCode e = (:) <$> timeStamp p <*> pure ps
                      | otherwise                        = (p :) <$> checkNewPiso ps e

checkAllPisos :: [PisoData] -> [PisoData] -> IO [PisoData]
checkAllPisos = foldM checkNewPiso
