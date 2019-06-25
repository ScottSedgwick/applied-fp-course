{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query), NamedParam(..))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Types                      (Comment, CommentText,
                                                     Error(..), Topic, fromDBComment, getCommentText, getTopic)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB = Sql.close . dbConn

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
    conn <- Sql.open fp
    Sql.execute_ conn createTableQ
    pure $ FirstAppDB conn
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DBComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DBComment to a Comment, we need to use ``fromDBComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments :: FirstAppDB -> Topic -> IO (Either Error [Comment])
getComments (FirstAppDB conn) topic = do
    eitherErrRows <- Sql.runDBAction (Sql.queryNamed conn sql [":topic" := getTopic topic])
    case eitherErrRows of
      (Left err) -> pure $ Left (DBError err)
      (Right xs) -> pure $ Right $ concat $ sequence $ map fromDBComment xs
  where
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = :topic"
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DBComment
  -- cannot be converted to a Comment, or simply ignoring any DBComment that is
  -- not valid.

promoteDbError :: Either SQLiteResponse a -> Either Error a
promoteDbError (Left err) = Left (DBError err)
promoteDbError (Right a)  = Right a

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic (FirstAppDB conn) topic comment = do
    currTime <- getCurrentTime
    res <- Sql.runDBAction (Sql.executeNamed conn sql 
                              [ ":topic" := getTopic topic
                              , ":comment" := getCommentText comment
                              , ":time" := currTime
                              ])
    pure $ promoteDbError res
  where
    sql = "INSERT INTO comments (topic,comment,time) VALUES (:topic, :comment, :time)"

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics (FirstAppDB conn) = do
    eitherErrRes <- Sql.runDBAction(Sql.query_ conn sql)
    case eitherErrRes of
      (Left err) -> pure $ Left $ DBError err
      (Right xs) -> pure $ Right xs
  where
    sql = "SELECT DISTINCT topic FROM comments"

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic (FirstAppDB conn) topic = do
    eitherErrUnit <- Sql.runDBAction (Sql.executeNamed conn sql [":topic" := getTopic topic])
    pure $ promoteDbError eitherErrUnit
  where
    sql = "DELETE FROM comments WHERE topic = :topic"
