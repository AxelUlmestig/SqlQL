{-# LANGUAGE OverloadedStrings #-}

module SQL.Format (
  format
) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text, pack)

import           SQL.AST

format :: SQL -> Text
format (Select cols from wher groupBy orderBy maybeLimit) =
  "select "
  <> formatSelectColumns cols
  <> maybe "" formatLimit maybeLimit

formatSelectColumns :: NonEmpty SelectColumn -> Text
formatSelectColumns = foldr (<>) "" . fmap ((<> ", ") . formatSelectColumn)

formatSelectColumn :: SelectColumn -> Text
formatSelectColumn SelectAll             = "*"
formatSelectColumn (SelectAllTable name) = name <> ".*"
formatSelectColumn (SelectColumn (Aliased exp mAlias)) = maybe formattedExp (formattedExp <>) ((" as " <>) <$> mAlias)
  where
    formattedExp = formatExpression exp

formatExpression :: Expression -> Text
formatExpression (ExpColumn col) = formatColumn col

formatColumn :: Column -> Text
formatColumn (Column col)          = col
formatColumn (TableColumn tbl col) = tbl <> "." <> col

formatLimit :: Limit -> Text
formatLimit (LimitAll)     = "limit all"
formatLimit (Limit number) = "limit " <> pack (show number)
