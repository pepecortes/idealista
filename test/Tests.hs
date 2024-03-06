{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Params
    ( loadConfig,
      AppConfig(apiKey),
      Params(Params, configFile, subcommand),
      Subcommand(Test) )
import ApiRequest ( authorizedRequest, requestSearch )

import Control.Monad.Reader ( ReaderT(runReaderT) )
import Test.Hspec ( SpecWith, describe, it, shouldReturn )
import Test.Tasty ( defaultMain, testGroup )
import Test.Tasty.Hspec ( testSpecs )
import System.Directory (getHomeDirectory)
import Network.HTTP.Client.Conduit (Request(requestHeaders))
import Data.Either (isRight, isLeft)
import Control.Monad (when)
import Database (getPisos)
import JsonWorker ( loadNewData )
import Data.Maybe (isJust)

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs [specTests]
  defaultMain (testGroup "All tests" [
                  testGroup "Specs" specs
              ])

specTests :: SpecWith ()
specTests = describe "idealista Hspec tests" $ do
  it "can access the database" $
    canGetArchives `shouldReturn` True
  it "~/.idealista/config should have apiKey" $
    haveApiKey `shouldReturn` True
  it "can get authorization from api" $
    canGetAuthorization `shouldReturn` True
  it "can get results from a search" $
    canGetATestSearch `shouldReturn` True
  it "cat get well formatted JSON data from a search" $
    canGetPisoData `shouldReturn` True

getConfig :: IO AppConfig
getConfig = do
  home <- getHomeDirectory
  let params = Params {configFile = home ++ "/.idealista/config", subcommand = Test}
  loadConfig $ configFile params

haveApiKey :: IO Bool
haveApiKey = do
  cfg <- getConfig
  pure $ apiKey cfg /= ""

canGetAuthorization :: IO Bool
canGetAuthorization = do
  cfg <- getConfig
  request <- runReaderT authorizedRequest cfg
  pure $ requestHeaders request /= []

canGetATestSearch :: IO Bool
canGetATestSearch = do
  cfg <- getConfig
  result <- runReaderT requestSearch cfg
  when (isLeft result) $ print result
  pure $ isRight result

canGetArchives :: IO Bool
canGetArchives = do
  cfg <- getConfig
  pisos <- runReaderT getPisos cfg
  putStr $ "pisos in database: " ++ show (length pisos) ++ " "
  pure $ not (null pisos)

canGetPisoData :: IO Bool
canGetPisoData = do
  cfg <- getConfig
  maybe_result <- runReaderT loadNewData cfg
  pure $ isJust maybe_result