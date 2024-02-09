module Archiver (updateArchive, loadArchiveDB) where

import Params ( AppConfig )
import PisoData ( PisoData )
import DatabaseBuild (getPisos, updatePisoData )
import Schemas (Piso)

import Control.Monad.Reader ( ReaderT, ReaderT )
import Database.Persist ( Entity(entityVal) )

updateArchive :: [PisoData] -> ReaderT AppConfig IO [Piso]
updateArchive = mapM updatePisoData

loadArchiveDB :: ReaderT AppConfig IO [Piso]
loadArchiveDB = map entityVal <$> getPisos
