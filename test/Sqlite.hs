--------------------------------------------------------------------------------
-- Column index support for persistent                                        --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the "Database.Persist.Index.Sqlite" module.
module Sqlite ( sqliteSpec ) where

--------------------------------------------------------------------------------

import Control.Concurrent (getNumCapabilities)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (runStderrLoggingT, defaultOutput)
import Control.Monad.Reader (ReaderT)

import Data.Text (Text)
import Data.Pool (Pool, destroyAllResources)

import Database.Sqlite
import Database.Persist.Index.Sqlite
import Database.Persist.Sqlite
import Database.Persist.TH

import System.Directory (removeFile)
import System.IO (stderr)

import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkEntityDefList "entities"] [persistLowerCase|
Example
    name String
    age Int
|]

-- | `countIndicesNamed` @name@ queries the SQLite database to count how many
-- indices named @name@ there are.
countIndicesNamed :: MonadIO m => Text -> ReaderT SqlBackend m [Single Int]
countIndicesNamed name =
    rawSql "SELECT count(*) FROM sqlite_master WHERE type='index' and name=?;"
        [PersistText name]

tests :: IO (Pool SqlBackend) -> TestTree
tests getPool = testGroup "Database.Persist.Index.Sqlite"
    [ testCase "Can create single-column index" $
        getPool >>= \pool -> flip runSqlPool pool $ do
            let indexColumns = [indexColumn Nothing ExampleName]
            let expectedIndexName = indexName indexColumns

            -- check that the index doesn't already exist
            startCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index already in database"
                [Single 0] startCount

            -- create the index
            runMigration $ createIndex defaultIndexOptions indexColumns

            -- check that it exists
            endCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index wasn't created" [Single 1] endCount
    , testCase "Can create multi-column index" $
        getPool >>= \pool -> flip runSqlPool pool $ do
            let indexColumns =
                    [ indexColumn Nothing ExampleName
                    , indexColumn Nothing ExampleAge
                    ]
            let expectedIndexName = indexName indexColumns

            -- check that the index doesn't already exist
            startCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index already in database"
                [Single 0] startCount

            -- create the index
            runMigration $ createIndex defaultIndexOptions indexColumns

            -- check that it exists
            endCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index wasn't created" [Single 1] endCount
    , testCase "Can create a unique index" $
        getPool >>= \pool -> flip runSqlPool pool $ do
            let indexColumns =
                    [ indexColumn (Just ASC) ExampleName
                    , indexColumn (Just DESC) ExampleAge
                    ]
            let expectedIndexName = indexName indexColumns <> "_unique"

            -- check that the index doesn't already exist
            startCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index already in database"
                [Single 0] startCount

            -- create the index
            let opts =
                    defaultIndexOptions{
                        idxName = Just expectedIndexName,
                        idxUnique = True
                    }
            runMigration $ createIndex opts indexColumns

            -- check that it exists
            endCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index wasn't created" [Single 1] endCount
    ]

initPool :: IO (Pool SqlBackend)
initPool = do
    threads <- getNumCapabilities
    pool <- runStderrLoggingT $ createSqlitePool "test.db" threads
    flip runSqlPool pool $ runMigrationQuiet (migrateModels entities)
    pure pool

closePool :: Pool SqlBackend -> IO ()
closePool pool = do
    destroyAllResources pool
    removeFile "test.db"

sqliteSpec :: TestTree
sqliteSpec = withResource initPool closePool tests

--------------------------------------------------------------------------------
