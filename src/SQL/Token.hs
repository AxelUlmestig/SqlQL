{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SQL.Token (
  Token(..)
) where

import qualified Data.List          as DL
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import qualified Data.Set           as Set
import           Data.Text          (Text)
import           Numeric.Natural    (Natural)
import qualified Text.Megaparsec    as P

-- TODO: store the source from Select etc to remember casing?
data Token = Select
           | From
           | Star
           | Comma
           | Dot
           | As
           | ProperNoun Text -- schema, table or column name
           | WhiteSpace Natural -- how many spaces
           | Newline
           | Comment Text
           deriving (Eq, Ord, Show)

instance P.VisualStream [Token] where
  showTokens _ = show

-- this is stab in the dark, how do you implement this properly?
instance P.TraversableStream [Token] where
  reachOffset o pst = (Nothing, pst)
