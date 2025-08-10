--------------------------------------------------------------------------------
-- Column index support for persistent                                        --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

-- | Provides support for creating search indices for SQLite databases.
module Database.Persist.Index.Sqlite (
    module Database.Persist.Index,
    Sqlite,
    createIndex,
    defaultIndexOptions
) where

--------------------------------------------------------------------------------

import Database.Persist ( PersistEntity )
import Database.Persist.Sql.Migration ( Migration )

import Database.Persist.Index hiding ( createIndex, defaultIndexOptions )
import qualified Database.Persist.Index as Index

--------------------------------------------------------------------------------

-- | Identifies SQLite on the type level.
data Sqlite

instance SupportsIndices Sqlite where
    type IndexExt Sqlite = ()
    type IndexColumnExt Sqlite = ()

    defaultIndexExtras :: IndexExt Sqlite
    defaultIndexExtras = ()

    defaultIndexColumnExtras :: IndexColumnExt Sqlite
    defaultIndexColumnExtras = ()

-- | `createIndex` is `Index.createIndex` specialised to `Sqlite`.
createIndex
    :: forall rec . PersistEntity rec
    => IndexOpts Sqlite
    -> [IndexColumnEx Sqlite rec]
    -> Migration
createIndex = Index.createIndex

-- | `defaultIndexOptions` is `Index.defaultIndexOptions`
-- specialised to `Sqlite`.
defaultIndexOptions :: IndexOpts Sqlite
defaultIndexOptions = Index.defaultIndexOptions

--------------------------------------------------------------------------------
