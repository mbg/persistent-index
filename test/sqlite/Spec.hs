--------------------------------------------------------------------------------
-- Column index support for persistent                                        --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the "Database.Persist.Index.Sqlite" module.
module Main ( main ) where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT)

import Data.Text (Text)

import Database.Persist.Index.Sqlite
import Database.Persist.Sqlite
import Database.Persist.TH

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

tests :: SqlBackend -> TestTree
tests backend = testGroup "Database.Persist.Index.Sqlite"
    [ testCase "Can create single-column index" $
        flip runSqlConn backend $ do
            let indexColumns = [indexColumn Nothing ExampleName]
            let expectedIndexName = indexName indexColumns

            -- check that the index doesn't already exist
            startCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index already in database"
                [Single 0] startCount

            -- create the index
            runMigration $ createIndex @Sqlite False indexColumns

            -- check that it exists
            endCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index wasn't created" [Single 1] endCount
    , testCase "Can create multi-column index" $
        flip runSqlConn backend $ do
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
            runMigration $ createIndex @Sqlite False indexColumns

            -- check that it exists
            endCount <- countIndicesNamed expectedIndexName
            liftIO $ assertEqual "Index wasn't created" [Single 1] endCount
    ]

-- | `main` is the main entry point for this test suite.
main :: IO ()
main = runStderrLoggingT $ withSqliteConn ":memory:" $ \backend -> do
    -- create the test table
    flip runSqlConn backend $ runMigrationQuiet (migrateModels entities)

    -- run the tests
    liftIO $ defaultMain (tests backend)

--------------------------------------------------------------------------------
