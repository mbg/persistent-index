--------------------------------------------------------------------------------
-- Column index support for persistent                                        --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Main ( main ) where

--------------------------------------------------------------------------------

import Test.Tasty

import Sqlite
import Postgresql

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "persistent-index"
    [ postgresSpec
    , sqliteSpec
    ]

-- | `main` is the main entry point for this test suite.
main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------
