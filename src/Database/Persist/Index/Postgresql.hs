--------------------------------------------------------------------------------
-- Column index support for persistent                                        --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

-- | Provides support for creating search indices for PostgreSQL databases.
module Database.Persist.Index.Postgresql (
    module Database.Persist.Index,
    Postgresql,
    PostgresqlIndexColumn,
    psqlNullsOrder,
    createIndex,
    defaultIndexOptions
) where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as T

import Database.Persist
import Database.Persist.Index hiding ( createIndex, defaultIndexOptions )
import qualified Database.Persist.Index as Index
import Database.Persist.Sql.Migration

--------------------------------------------------------------------------------

-- | Identifies PostgreSQL on the type level.
data Postgresql

-- | Represents index column options that are specific to PostgreSQL.
data PostgresqlIndexColumn = PsqlIndexColumn {
    -- | Controls whether @NULL@ values appear as the first or last elements of
    -- the index.
    psqlNullsOrder :: Maybe NullsOrder
}

instance SupportsIndices Postgresql where
    type IndexExt Postgresql = ()
    type IndexColumnExt Postgresql = PostgresqlIndexColumn

    defaultIndexExtras :: IndexExt Postgresql
    defaultIndexExtras = ()

    defaultIndexColumnExtras :: IndexColumnExt Postgresql
    defaultIndexColumnExtras = PsqlIndexColumn{
        psqlNullsOrder = Nothing
    }

    createIndex
        :: forall rec . PersistEntity rec
        => IndexOpts Postgresql
        -> [IndexColumnEx Postgresql rec]
        -> Migration
    createIndex opts columns = addMigration False $ T.concat
        [ "CREATE "
        , if idxUnique opts then "UNIQUE " else T.empty
        , "INDEX IF NOT EXISTS "
        , fromMaybe (indexName columns) (idxName opts), " ON "
        , tableName, " (", T.intercalate ", " fieldSql, ") "
        ]
        where
            fieldSql = map mkSql columns
            tableName =
                unEntityNameDB . getEntityDBName $
                entityDef (Proxy :: Proxy rec)
            sortOrder IdxColumn{..} = case idxColumnSortOrder of
                Nothing -> T.empty
                Just order -> sortOrderSql order

            mkSql col = T.concat ["\"", indexColumnName col, "\" ", sortOrder col]

-- | `createIndex` is `Index.createIndex` specialised to `Postgresql`.
createIndex
    :: forall rec . PersistEntity rec
    => IndexOpts Postgresql
    -> [IndexColumnEx Postgresql rec]
    -> Migration
createIndex = Index.createIndex

-- | `defaultIndexOptions` is `Index.defaultIndexOptions`
-- specialised to `Postgresql`.
defaultIndexOptions :: IndexOpts Postgresql
defaultIndexOptions = Index.defaultIndexOptions

--------------------------------------------------------------------------------
