{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404, methodGet, methodPost)

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString          as BS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (ContentType(..), Error(..), RqType(..),
                                           getCommentText, getTopic, mkCommentText, mkTopic,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> Text
  -> Response
mkResponse st ct xs = responseLBS st [(hContentType, renderContentType ct)] (textToLazyByteString xs)

-- responseLBS :: Status -> ResponseHeaders -> ByteString -> Response
-- HeaderName = CI ByteString

resp200
  :: ContentType
  -> Text
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> Text
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> Text
  -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topic comment = do
    t <- mkTopic topic
    c <- mkCommentText (lazyByteStringToStrictText comment)
    pure (AddRq t c)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic = do
  t <- mkTopic topic
  pure (ViewRq t)

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse xs = resp400 PlainText msg
  where
    msg = case xs of
            (NoTopic t)    -> "No Topic called " <> (getTopic t) <> " found"
            EmptyTopic     -> "Empty Topic Text"
            EmptyComment   -> "Empty Comment Text"
            InvalidRequest -> "Invalid Request"

textToLazyByteString :: Text -> LBS.ByteString
textToLazyByteString = LBS.pack . BS.unpack . encodeUtf8

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq = 
  case (requestMethod rq, pathInfo rq) of
    ("POST", [t,"add"])  -> strictRequestBody rq >>= \c -> pure $ mkAddRequest t c
    ("GET", [t, "view"]) -> pure $ mkViewRequest t
    ("GET", ["list"])    -> pure $ mkListRequest
    _                    -> pure $ Left InvalidRequest
    
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest rq = Right $ resp200 PlainText msg
  where
    msg = case rq of
            (AddRq t c) -> "Added Topic \"" <> (getTopic t) <> "\" with comment \"" <> (getCommentText c) <> "\""
            (ViewRq t)  -> "Viewed Topic \"" <> (getTopic t) <> "\""
            ListRq      -> "Here is a list"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app rq cb = mkRequest rq >>= cb . tryResp . tryReq

tryReq :: Either Error RqType -> Either Error Response
tryReq (Left err) = Left err
tryReq (Right rq) = handleRequest rq

tryResp :: Either Error Response -> Response
tryResp (Left err) = mkErrorResponse err
tryResp (Right rs) = rs

--   app
--   :: Request
--   -> (Response -> IO ResponseReceived)
--   -> IO ResponseReceived
-- app _ cb = cb $ responseLBS status200 [] "Hello World"

runApp :: IO ()
runApp = run 3000 app
