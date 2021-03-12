module SQL (
  parse,
  format
) where

import           Data.Text       (Text)
import           Data.Void       (Void)
import qualified Text.Megaparsec as P

import           SQL.AST         (SQL)
import           SQL.Format      (format)
import           SQL.Parser      (sqlP)

parse :: Text -> Either (P.ParseErrorBundle Text Void) SQL
parse = P.parse sqlP "name of source file"
