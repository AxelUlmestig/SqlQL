{-# LANGUAGE OverloadedStrings #-}

module SQL.Format (
  format
) where

import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Text          (Text, pack)

import           SQL.AST

format :: SQL -> Text
format (Select cols from wher maybeGroupBy maybeOrderBy maybeLimit) =
  "select "
  <> formatSelectColumns cols
  <> " "
  <> maybe "" formatGroupBy maybeGroupBy
  <> maybe "" formatOrderBy maybeOrderBy
  <> " "
  <> maybe "" formatLimit maybeLimit

formatSelectColumns :: NonEmpty SelectColumn -> Text
formatSelectColumns = formatCommaSeparated formatSelectColumn . toList

formatSelectColumn :: SelectColumn -> Text
formatSelectColumn SelectAll = "*"
formatSelectColumn (SelectAllTable name) = name <> ".*"
formatSelectColumn (SelectColumn (Aliased exp mAlias)) = maybe formattedExp (formattedExp <>) ((" as " <>) <$> mAlias)
  where
    formattedExp = formatExpression exp

formatExpression :: Expression -> Text
formatExpression (ExpTrue)                     = "true"
formatExpression (ExpFalse)                    = "false"
formatExpression (ExpString str)               = "'" <> str <> "'"
formatExpression (ExpInt num)                  = pack (show num)
formatExpression (ExpFloat num)                = pack (show num)
formatExpression (ExpColumn col)               = formatColumn col
formatExpression (ExpFunc fName args)          = fName <> "(" <> formatCommaSeparated formatExpression args <> ")"
formatExpression (MetaExpression op exp1 exp2) = undefined -- TODO
formatExpression (ParensedExp exp)             = "(" <> formatExpression exp <> ")"

formatColumn :: Column -> Text
formatColumn (Column col)          = col
formatColumn (TableColumn tbl col) = tbl <> "." <> col

formatLimit :: Limit -> Text
formatLimit (LimitAll)     = "limit all"
formatLimit (Limit number) = "limit " <> pack (show number)

formatGroupBy :: GroupBy -> Text
formatGroupBy (GroupBy (groups)) =
  "group by "
  <> formatCommaSeparated formatExpression (toList groups)
  <> " "

formatOrderBy :: OrderBy -> Text
formatOrderBy (OrderBy (orders)) =
  "order by "
  <> formatCommaSeparated formatOrderExpression (toList orders)

formatOrderExpression :: OrderExpression -> Text
formatOrderExpression (OrderExpression (exp) maybeDir) =
  formatExpression exp
  <> maybe "" (fmap (\x -> " " <> x) formatOrderDirection) maybeDir

formatOrderDirection :: OrderDirection -> Text
formatOrderDirection (Asc)  = "asc"
formatOrderDirection (Desc) = "desc"

formatCommaSeparated :: (a -> Text) -> [a] -> Text
formatCommaSeparated f (h:t) = f h <> (foldr (<>) "" . fmap ((", " <>) . f)) t
formatCommaSeparated f ([])  = ""
