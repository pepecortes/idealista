{-# LANGUAGE RecordWildCards #-}

module Marshalling (toPisoRecord) where
  
import Schemas ( Piso(..) )
import PisoData ( PisoData(..) )
import Prelude hiding (floor)

import Data.Time (getCurrentTime)

toPisoRecord :: PisoData -> IO Piso
toPisoRecord PisoData {..} = do
  now <- getCurrentTime
  pure Piso {
              pisoPropertyCode = propertyCode
            , pisoUrl = url
            , pisoExterior = exterior
            , pisoFloor =  floor
            , pisoPrice = fromIntegral price
            , pisoRooms = fromIntegral rooms
            , pisoSize = fromIntegral size
            , pisoStatus = status
            , pisoHasLift = hasLift
            , pisoDescription = description
            , pisoFirstSeen = Just now
            , pisoRecentSeen = Just now
            , pisoLastPrice = Just $ fromIntegral price
            }