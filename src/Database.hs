{-# LANGUAGE OverloadedStrings #-}

module Database 
    ( db
    , DatabaseAction
    ) where

import Data.Text (Text ())
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Conduit
import Database.Persist.Sqlite

type DatabaseAction a = SqlPersistT (NoLoggingT (ResourceT IO)) a

connectionString :: Text
connectionString = "database.sqlite"

db :: DatabaseAction a -> IO a
db = runSqlite connectionString
