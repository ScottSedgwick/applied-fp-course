{-# LANGUAGE OverloadedStrings #-}
module Level05.Conf
    ( Conf (..)
    , firstAppConfig
    ) where

data Conf = Conf
  { dbFilePath :: FilePath
  , appPort :: Int
  }

firstAppConfig :: Conf
firstAppConfig = Conf { dbFilePath = "app_db.db", appPort = 3000 }
