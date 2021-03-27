module SQL (
  parse,
  parseTest,
  format
) where

import           Data.Text       (Text)
import           Data.Void       (Void)
import qualified Text.Megaparsec as P

import           SQL.AST         (SQL)
import           SQL.Format      (format)
import           SQL.Parser      (sqlP)

import           SQL.Lexer       (tokensP)

-- temporary ugly solution of nested Either
parse src = P.parse sqlP "name of source file" <$> P.parse tokensP "name of source file" src

-- parse :: Text -> Either (P.ParseErrorBundle Text Void) SQL
-- parse src = do
--   tokens <- P.parse tokensP "name of source file" src
--   P.parse sqlP "name of source file" tokens

-- parseTest :: Text -> IO ()
-- parseTest = P.parseTest sqlP

parseTest :: Text -> IO ()
parseTest input = do
  case P.parse tokensP "" input of
    Left e        -> putStr (P.errorBundlePretty e)
    Right tokens  -> do
      case P.parse sqlP "" tokens of
        -- P.errorBundlePretty requires [Token] to implement P.TraversableStream
        Left e    -> putStr (P.errorBundlePretty e)
        Right sql -> print sql
