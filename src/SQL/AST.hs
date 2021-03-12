module SQL.AST (
  SQL(..),
  Column(..),
  SelectColumn(..),
  From(..),
  Join(..),
  Where(..),
  GroupBy(..),
  Expression(..),
  ExpressionOperator(..),
  OrderBy(..),
  OrderExpression(..),
  OrderDirection(..),
  Limit(..),
  Aliased(..)
) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text)

type TableName = Text
type ColumnName = Text

data SQL = Select (NonEmpty SelectColumn) (Maybe From) (Maybe Where) (Maybe GroupBy) (Maybe OrderBy) (Maybe Limit)
         deriving (Show)

-- select
data SelectColumn = SelectColumn (Aliased Expression)
                  | SelectAll
                  | SelectAllTable TableName
                  deriving (Show)

data Column = Column ColumnName
            | TableColumn TableName ColumnName
            deriving (Show)

data Aliased a = Aliased a (Maybe Text)
               deriving (Show)

-- TODO: is [not] null
data Expression = ExpTrue
                | ExpFalse
                | ExpString Text
                | ExpInt Int
                | ExpFloat Float
                | MetaExpression ExpressionOperator Expression Expression
                | ExpColumn Column
                | ParensedExp Expression
                deriving (Show)

data ExpressionOperator = ExpAnd
                        | ExpOr
                        | ExpAdd
                        | ExpSubtract
                        | ExpMultiply
                        | ExpDivide
                        | ExpEqual
                        | ExpGreaterThan
                        | ExpGreaterThanEqual
                        | ExpLesserThan
                        | ExpLesserThanEqual
                        | ExpExponentiate
                        deriving (Show)

-- from
data From = From (Aliased TableName) [Join]
          deriving (Show)

data Join = InnerJoin (Aliased TableName) Expression
          | LeftJoin (Aliased TableName) Expression
          | CrossJoin (Aliased TableName)
          deriving (Show)

-- where
data Where = Where Expression
           deriving (Show)

-- group by
-- TODO: 'group by ()' is technically valid SQL
data GroupBy = GroupBy (NonEmpty Expression)
             deriving (Show)

-- order by
data OrderBy = OrderBy (NonEmpty OrderExpression)
             deriving (Show)

data OrderExpression = OrderExpression Expression (Maybe OrderDirection)
                     deriving (Show)

data OrderDirection = Asc | Desc
                    deriving (Show)

-- limit
data Limit = Limit Int | LimitAll
           deriving (Show)

-- TODO: group by

