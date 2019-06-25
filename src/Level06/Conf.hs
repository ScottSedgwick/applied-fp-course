{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)

import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Bifunctor           (first)
import           Data.Monoid              (Last(..), (<>))

import           Level06.AppM             (AppM, liftEither)
import           Level06.Types            (Conf(..), ConfigError(..),
                                           DBFilePath (DBFilePath), PartialConf(..),
                                           Port (Port))
import           Waargonaut.Decode.Error  (DecodeError(..))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf :: PartialConf
defaultConf = PartialConf
  { pcPort = Last (Just (Port 3000))
  , pcDBFilePath = Last (Just (DBFilePath "app_db.db"))
  }

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig :: PartialConf -> Either ConfigError Conf
makeConfig pc =
  let
    mport = getLast (pcPort pc)
    mpath = getLast (pcDBFilePath pc)
    mkcfg Nothing _ = Left (BadConfFile (KeyNotFound "Port not configured"))
    mkcfg _ Nothing = Left (BadConfFile (KeyNotFound "DB File Path not configured"))
    mkcfg (Just port) (Just path) = Right (Conf port path)
  in
    mkcfg mport mpath

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions :: FilePath -> AppM ConfigError Conf
parseOptions fp = do
  fileConf <- parseJSONConfigFile fp
  lineConf <- liftIO commandLineParser
  let pConf = defaultConf <> fileConf <> lineConf
  liftEither (makeConfig pConf)