{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE QuasiQuotes                  #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE DerivingStrategies           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE StandaloneDeriving           #-}
{-# LANGUAGE UndecidableInstances         #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE RecordWildCards #-}

module Schemas where
import Data.Time ( UTCTime )
import Data.Text ( Text )

import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Piso
  propertyCode    Text
  url             Text              default="url"
  exterior        Bool              default=True
  floor           Text Maybe        default="floor"
  price           Int               default=100
  rooms           Int               default=1
  size            Int               default=50
  status          String            default="ok"
  hasLift         Bool              default=False
  description     Text              default="None"
  firstSeen       UTCTime Maybe     default=CURRENT_TIME
  recentSeen      UTCTime Maybe     default=CURRENT_TIME
  lastPrice       Int Maybe         default=666
  UniquePropertyCode  propertyCode
  deriving (Show)
|]



