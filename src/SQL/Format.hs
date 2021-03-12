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

formatLimit :: Limit -> Text
formatLimit (LimitAll) = "limit all"
formatLimit (Limit number) = "limit " <> pack (show number)
