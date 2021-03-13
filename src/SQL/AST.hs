module SQL.AST (
  SQL(..),
  Column(..),
  SelectColumn(..),
  From(..),
  Join(..),
  JoinCondition(..),
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
type FunctionName = Text

data SQL = Select (NonEmpty SelectColumn) (Maybe From) (Maybe Where) (Maybe GroupBy) (Maybe OrderBy) (Maybe Limit)
         deriving (Eq, Show)

-- select
-- TODO: distinct on
data SelectColumn = SelectColumn (Aliased Expression)
                  | SelectAll
                  | SelectAllTable TableName
                  deriving (Eq, Show)

data Column = Column ColumnName
            | TableColumn TableName ColumnName
            deriving (Eq, Show)

data Aliased a = Aliased a (Maybe Text)
               deriving (Eq, Show)

-- TODO: is [not] null
-- TODO: not
data Expression = ExpTrue
                | ExpFalse
                | ExpString Text
                | ExpInt Int
                | ExpFloat Float
                | ExpColumn Column
                | ExpFunc FunctionName [Expression]
                | MetaExpression ExpressionOperator Expression Expression
                | ParensedExp Expression
                deriving (Eq, Show)

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
                        deriving (Eq, Show)

-- from
data From = From (Aliased TableName) [Join]
          deriving (Eq, Show)

data Join = InnerJoin (Aliased TableName) JoinCondition
          | LeftJoin  (Aliased TableName) JoinCondition
          | RightJoin (Aliased TableName) JoinCondition
          | FullJoin (Aliased TableName) JoinCondition
          | CrossJoin (Aliased TableName)
          deriving (Eq, Show)

data JoinCondition = OnJoinCondition Expression
                   | UsingJoinCondition (NonEmpty ColumnName)
                   | NaturalJoinCondition
                   deriving (Eq, Show)

-- where
data Where = Where Expression
           deriving (Eq, Show)

-- group by
-- TODO: 'group by ()' is technically valid SQL
data GroupBy = GroupBy (NonEmpty Expression)
             deriving (Eq, Show)

-- order by
data OrderBy = OrderBy (NonEmpty OrderExpression)
             deriving (Eq, Show)

data OrderExpression = OrderExpression Expression (Maybe OrderDirection)
                     deriving (Eq, Show)

data OrderDirection = Asc | Desc
                    deriving (Eq, Show)

-- limit
data Limit = Limit Int | LimitAll
           deriving (Eq, Show)

-- TODO: group by

