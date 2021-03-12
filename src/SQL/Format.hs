{-# LANGUAGE OverloadedStrings #-}

module SQL.Format (
  format
) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text)

import           SQL.AST

format :: SQL -> Text
format (Select cols from wher groupBy orderBy limit) =
  "select "
  <> formatSelectColumns cols

formatSelectColumns :: NonEmpty SelectColumn -> Text
formatSelectColumns = foldr (<>) "" . fmap ((<> ", ") . formatSelectColumn)

formatSelectColumn :: SelectColumn -> Text
formatSelectColumn SelectAll             = "*"
formatSelectColumn (SelectAllTable name) = name <> ".*"
