{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database (getPisos, updatePisoData) where

import Schemas
    ( Piso,
      EntityField(PisoRecentSeen, PisoLastPrice),
      Unique(UniquePropertyCode) )
import Data.Text ( Text, pack )
import PisoData ( PisoData(..) )
import Marshalling (toPisoRecord)
import Params (AppConfig (..))

import Data.Time (getCurrentTime)
import Database.Persist
    ( Entity(Entity),
      selectList,
      insertRecord,
      PersistStoreWrite(updateGet),
      PersistUniqueRead(getBy),
      (=.) )
import Database.Persist.Sqlite ( runSqlite )
import Control.Monad.IO.Unlift ( MonadUnliftIO, liftIO )
import Control.Monad.Reader ( ReaderT, asks )


-- Only used during the creation of the database
{- 
buildDb :: MonadUnliftIO m => ReaderT AppConfig m ()
buildDb = do
  path <- asks databasePath
  runSqlite (pack path) $ runMigration migrateAll
-}


getPisos :: MonadUnliftIO m => ReaderT AppConfig m [Entity Piso]
getPisos = do
  path <- asks databasePath
  runSqlite (pack path) $ selectList [] []

getPiso :: MonadUnliftIO m => Text -> ReaderT AppConfig m (Maybe (Entity Piso))
getPiso propertyCode = do
  path <- asks databasePath
  runSqlite (pack path) $ getBy $ UniquePropertyCode propertyCode

updatePisoData :: (MonadUnliftIO m) => PisoData -> ReaderT AppConfig m Piso
updatePisoData pisodata@PisoData{propertyCode} = do
  record <- getPiso propertyCode
  case record of
    Nothing -> insertPisoData pisodata
    Just pisoEntity -> refreshPisoData pisoEntity pisodata

insertPisoData :: MonadUnliftIO m => PisoData -> ReaderT AppConfig m Piso
insertPisoData pisodata = do
  path <- asks databasePath
  pisoRecord <- liftIO $ toPisoRecord pisodata
  runSqlite (pack path) $ insertRecord pisoRecord

refreshPisoData :: MonadUnliftIO m => Entity Piso -> PisoData -> ReaderT AppConfig m Piso
refreshPisoData (Entity key _) PisoData{price}= do
  now <- liftIO getCurrentTime
  path <- asks databasePath
  let lastPrice = fromIntegral price
  let updates = [
                  PisoLastPrice  =. Just lastPrice
                , PisoRecentSeen =. Just now
                ]
  runSqlite (pack path) $ updateGet key updates