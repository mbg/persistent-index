--------------------------------------------------------------------------------
-- Column index support for persistent                                        --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides support for creating search indices for SQLite databases.
module Database.Persist.Index.Sqlite (
    module Database.Persist.Index,
    Sqlite
) where

--------------------------------------------------------------------------------

import Database.Persist.Index

--------------------------------------------------------------------------------

-- | Identifies SQLite on the type level.
data Sqlite

instance SupportsIndices Sqlite where
    type IndexColumnExt Sqlite = ()

    defaultIndexExtras :: IndexColumnExt Sqlite
    defaultIndexExtras = ()

--------------------------------------------------------------------------------
