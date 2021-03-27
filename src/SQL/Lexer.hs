module SQL.Lexer (
  tokensP
) where

import           Control.Applicative       (many, some, (<|>))
import           Control.Monad.Combinators (manyTill)
import           Data.Text                 (Text, pack)
import           Data.Void                 (Void)
import           GHC.Natural               (intToNatural)
import           Text.Megaparsec           (Parsec, anySingle, eof, lookAhead,
                                            satisfy, try)
import           Text.Megaparsec.Char      (alphaNumChar, char, letterChar,
                                            newline, spaceChar, string')

import           SQL.Token                 (Token (..))

type Lexer = Parsec Void Text

tokensP :: Lexer [Token]
tokensP = many tokenP

tokenP :: Lexer Token
tokenP =    try (Select <$ string' "select" <* lookAhead spaceChar)
       <|>  try (From <$ string' "from" <* lookAhead spaceChar)
       <|>  try (As <$ string' "as" <* lookAhead spaceChar)
       <|>  Star <$ char '*'
       <|>  ProperNoun . pack <$> (
              (:)
              <$> (letterChar <|> char '_')
              <*> many (alphaNumChar <|> char '_')
            )
       <|>  Comma <$ char ','
       <|>  Dot <$ char '.'
       <|>  WhiteSpace . intToNatural . length <$> some (char ' ')
       <|>  Newline <$ newline
       <|>  Comment . pack <$ string' "--" <*> manyTill anySingle newline -- what if the file ends?
