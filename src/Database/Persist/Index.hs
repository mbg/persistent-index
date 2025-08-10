--------------------------------------------------------------------------------
-- Column index support for persistent                                        --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a base module containing types and functions related to database
-- indices, which are shared between different DBMS. You probably want to
-- import a DBMS-specific module, such as "Database.Persist.Index.Postgresql"
-- instead of importing this one directly.
module Database.Persist.Index (
    SortOrder(..),
    sortOrderSql,
    NullsOrder(..),
    nullsOrderSql,
    IndexColumnEx(..),
    SupportsIndices(..),
    indexColumn,
    IndexColumn,
    indexColumnName,
    indexName
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import qualified Data.Text as T
import Data.Proxy

import Database.Persist
import Database.Persist.Sql.Migration

--------------------------------------------------------------------------------

-- | Enumerates sorting orders.
data SortOrder
    -- | Ascending sort order.
    = ASC
    -- | Descending sort order.
    | DESC
    deriving (Eq, Show)

-- | `sortOrderSql` @sortOrder@ converts @sortOrder@ to the corresponding
-- SQL string as a `T.Text` value.
sortOrderSql :: SortOrder -> T.Text
sortOrderSql ASC = "ASC"
sortOrderSql DESC = "DESC"

-- | Enumerates different orders for where @NULL@ values should appear.
data NullsOrder
    -- | @NULL@ values should appear as the first elements in the index.
    = NullsFirst
    -- | @NULL@ values should appear as the last elements in the index.
    | NullsLast
    deriving (Eq, Show)

-- | `nullsOrderSql` @nullsOrder@ converts @nullsOrder@ to the corresponding
-- SQL string as a `T.Text` value.
nullsOrderSql :: NullsOrder -> T.Text
nullsOrderSql NullsFirst = "NULLS FIRST"
nullsOrderSql NullsLast = "NULLS LAST"

--------------------------------------------------------------------------------

-- | Represents a column for inclusion in an index. The phantom type
-- parameter @rec@ is used to ensure that the fields belong to the table
-- for which the index is created.
data IndexColumnEx dbms rec
    = forall typ . IdxColumn {
        -- | The column.
        idxField :: !(EntityField rec typ),
        -- | The order in which values of this column should appear
        -- in the index.
        idxSortOrder :: !(Maybe SortOrder),
        -- | Additional, DBMS-specific data.
        idxExtra :: !(IndexColumnExt dbms)
    }

-- | Like `IndexColumnEx`, but with no DBMS-specific data.
type IndexColumn = IndexColumnEx ()

-- | A class for database engines that support search indices.
class SupportsIndices dbms where
    type family IndexColumnExt dbms :: Type

    defaultIndexExtras :: IndexColumnExt dbms

    createIndex
        :: forall rec . PersistEntity rec
        => Bool
        -> [IndexColumnEx dbms rec]
        -> Migration
    createIndex unique columns = addMigration False $ T.concat
        [ "CREATE "
        , if unique then "UNIQUE " else T.empty
        , "INDEX IF NOT EXISTS "
        , indexName columns, " ON "
        , tableName, " (", T.intercalate ", " fieldSql, ") "
        ]
        where
            fieldSql = map mkSql columns
            tableName =
                unEntityNameDB . getEntityDBName $
                entityDef (Proxy :: Proxy rec)
            sortOrder IdxColumn{..} = case idxSortOrder of
                Nothing -> T.empty
                Just order -> sortOrderSql order

            mkSql col = T.concat ["\"", indexColumnName col, "\" ", sortOrder col]

-- | `indexColumn` @sortOrder entityField@ constructs an `IndexColumn`
-- for @entityField@ where the optional @sortOrder@ determines
-- the order of values in the index.
indexColumn
    :: forall dbms rec typ . (SupportsIndices dbms, PersistEntity rec)
    => Maybe SortOrder
    -> EntityField rec typ
    -> IndexColumnEx dbms rec
indexColumn mSortOrder entityField = IdxColumn{
    idxField = entityField,
    idxSortOrder = mSortOrder,
    idxExtra = defaultIndexExtras @dbms
}

-- | `indexColumnName` @indexColumn@ retrieves the name of the column
-- represented by @indexColumn@.
indexColumnName :: PersistEntity rec => IndexColumnEx dbms rec -> T.Text
indexColumnName IdxColumn{..} =
    unFieldNameDB . fieldDB . persistFieldDef $ idxField

-- | `indexName` @indexColumns@ generates the default name for an index
-- comprised of @indexColumns@. For example, assuming that @indexColumns@
-- contains two columns named @"name"@ and @"age"@, which belong to a table
-- named @"person"@, the name generated by this function would be
-- @"person_name_age_idx"@.
indexName
    :: forall rec dbms . PersistEntity rec
    => [IndexColumnEx dbms rec] -> T.Text
indexName columns =
    T.concat [ tableName, "_", T.intercalate "_" fieldNames, "_idx" ]
    where
        tableName :: T.Text
        tableName =
            unEntityNameDB . getEntityDBName $
            entityDef (Proxy :: Proxy rec)

        fieldNames :: [T.Text]
        fieldNames = map indexColumnName columns

--------------------------------------------------------------------------------
