{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Except       (MonadError (..))

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Attoparsec      as WA
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))
import           Waargonaut.Types.JObject   (parseJObject)

import           Level06.AppM               (AppM (runAppM))
import           Level06.Types              (ConfigError (BadConfFile),
                                             PartialConf (PartialConf),
                                             partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- *** Exception: badFileName.no: openBinaryFile: does not exist (No such file or directory)
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\"foo\":33}\n"
--
readConfFile :: FilePath -> AppM ConfigError ByteString
readConfFile fp = liftIO (BS.readFile fp) `catchError` throwError

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.

parseJSONConfigFile :: FilePath -> AppM ConfigError PartialConf
parseJSONConfigFile fp = do
  inp <- readConfFile fp
  case (WA.pureDecodeAttoparsecByteString partialConfDecoder inp) of
    Left (de,_) -> throwError (BadConfFile de)
    Right p -> pure p

-- Go to 'src/Level06/Conf.hs' next.
