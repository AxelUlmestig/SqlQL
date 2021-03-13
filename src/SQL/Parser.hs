{-# LANGUAGE OverloadedStrings #-}

module SQL.Parser (
  sqlP
) where

import           Prelude                            hiding (tail)

import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.Functor                       (($>))
import           Data.List.NonEmpty                 (NonEmpty)
import           Data.Text                          (Text, breakOn, pack, tail)
import           Data.Tuple                         (swap)
import           Data.Void                          (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

import           SQL.AST

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

sqlP :: Parser SQL
sqlP = lexeme $ space
                $>  Select
                <*  lexeme (string' "select")
                <*> optional distinctP
                <*> selectColumnsP
                <*> optional fromP
                <*> optional whereP
                <*> optional groupByP
                <*> optional orderByP
                <*> optional limitP
                <* eof

-- select
distinctP :: Parser Distinct
distinctP = lexeme $ distinctOnP <|> simpleDistinctP
  where
    distinctOnP     = lexeme $
                        string' "distinct on"
                        <*  space
                        $>  DistinctOn
                        <*> between (lexeme (char '(')) (lexeme (char ')')) (NE.sepBy1 (lexeme expressionP) (lexeme (char ',')))
    simpleDistinctP = lexeme $
                        string' "distinct"
                        $> Distinct



selectColumnsP :: Parser (NonEmpty SelectColumn)
selectColumnsP = lexeme $ NE.sepBy1 selectColumnP (lexeme (char ','))

selectColumnP :: Parser SelectColumn
selectColumnP = lexeme $
                  starP
                  <|> try tableStarP
                  <|> (SelectColumn <$> alias expressionP)
  where
    starP       = SelectAll <$ char '*'
    tableStarP  = SelectAllTable <$> tableNameP <* string ".*"

columnP :: Parser Column
columnP = do
  x <- columnNameP
  y <- optional (char '.' *> columnNameP)
  return $ maybe (Column x) (TableColumn x) y

expressionP :: Parser Expression
expressionP = do
                maybeParens <- optional (char '(')
                case maybeParens of
                  Nothing -> do
                    exp <- singleExpP
                    combinedExpP exp
                  (Just _) -> do
                    exp <- expressionP <* char ')'
                    combinedExpP (ParensedExp exp)
  where
    combinedExpP exp1 = do
                          maybeOp <- optional expOpP
                          case maybeOp of
                            Nothing -> return exp1
                            (Just op) -> do
                              exp2 <- expressionP
                              combinedExpP (MetaExpression op exp1 exp2)

    singleExpP = lexeme $
                   ExpTrue <$ string' "true"
                   <|> ExpFalse <$ string' "false"
                   <|> ExpString . pack <$ char '\'' <*> manyTill anySingle (char '\'')
                   <|> try (ExpFunc <$> columnNameP <*> between (lexeme (char '(')) (lexeme (char ')')) (sepBy expressionP (lexeme (char ','))))
                   <|> ExpColumn <$> columnP
                   <|> ExpInt <$> L.signed space L.decimal
                   <|> ExpFloat <$> L.signed space L.float

    expOpP :: Parser ExpressionOperator
    expOpP = lexeme $
               ExpAnd                   <$ string' "and" <* space1
               <|> try (ExpOr           <$ string' "or"  <* space1)
               <|> ExpAdd               <$ char    '+'
               <|> ExpSubtract          <$ char    '-'
               <|> ExpMultiply          <$ char    '*'
               <|> ExpDivide            <$ char    '/'
               <|> ExpEqual             <$ char    '='
               <|> ExpGreaterThan       <$ char    '>'
               <|> ExpGreaterThanEqual  <$ string' ">="
               <|> ExpLesserThan        <$ char    '<'
               <|> ExpLesserThanEqual   <$ string' "<="
               <|> ExpExponentiate      <$ char    '^'

columnNameP :: Parser Text
columnNameP = fmap pack $
              (:)
              <$> (letterChar <|> char '_')
              <*> many (alphaNumChar <|> char '_')

-- from
fromP :: Parser From
fromP = From
        <$  lexeme (string' "from" <* space1)
        <*> alias tableNameP
        <*> many joinP

tableNameP :: Parser Text
tableNameP = columnNameP

-- TODO: handle inner join without inner
-- join b on a.id = b.id
-- join b using(id)
joinP :: Parser Join
joinP = lexeme (innerJoinP <|> leftJoinP <|> rightJoinP <|> crossJoinP)
  where
    innerJoinP = InnerJoin
                 <$  lexeme (string' "inner join" <* space1)
                 <*> alias tableNameP
                 <*> joinConditionP
                 -- <|>  lexeme (string' "using" <* space1)
                 -- <*> expressionP
    leftJoinP =  LeftJoin
                 <$  lexeme (string' "left join" <* space1)
                 <*> alias tableNameP
                 <*> joinConditionP
    rightJoinP =  RightJoin
                 <$  lexeme (string' "right join" <* space1)
                 <*> alias tableNameP
                 <*> joinConditionP
    crossJoinP = CrossJoin
                 <$  lexeme (string' "cross join" <* space1)
                 <*> alias tableNameP

joinConditionP :: Parser JoinCondition
joinConditionP = lexeme (onJoinConditionP <|> usingJoinConditionP)
  where
    onJoinConditionP = OnJoinCondition
                      <$ string' "on"
                      <* space1
                      <*> expressionP
    usingJoinConditionP = UsingJoinCondition
                      <$  string' "using"
                      <*  space
                      <*> between (lexeme (char '(')) (lexeme (char ')')) (NE.sepBy1 (lexeme columnNameP) (lexeme (char ',')))

-- where
whereP :: Parser Where
whereP = lexeme $
           Where
           <$  lexeme (string' "where" <* space1)
           <*> expressionP

-- group by
groupByP :: Parser GroupBy
groupByP = lexeme $
             string' "group by"
             <*  space1
             <*  space
             $>  GroupBy
             <*> NE.sepBy1 expressionP (lexeme (char ','))

-- order by
orderByP :: Parser OrderBy
orderByP = lexeme $
             string' "order by"
             <*  space1
             $>  OrderBy
             <*> NE.sepBy1 orderExpressionP (lexeme (char ','))

orderExpressionP :: Parser OrderExpression
orderExpressionP = OrderExpression
                   <$> expressionP
                   <*> optional orderDirectionP
  where
    orderDirectionP = (Asc <$ string' "asc")
                      <|> (Desc <$ string' "desc")

-- limit
limitP :: Parser Limit
limitP = lexeme $
           string' "limit"
           <* space1
           <* space
           *> (
             (Limit <$> L.decimal)
             <|> (LimitAll <$ string' "all")
           )

-- TODO: what about schemas?
alias :: Parser a -> Parser (Aliased a)
alias p = lexeme $ do
  x <- lexeme p
  a <- optional (lexeme (string' "as ") *> columnNameP)
  return (Aliased x a)
