{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Base type for graphs-}
module GraphTypes(TextEdge(..)) where

import RIO hiding (ByteString)
import RIO.Text (Text)
import qualified RIO.Set as Set
import qualified Prelude as P
import qualified RIO.Map as Map

-- | A set of edges, connected to a set of Text nodes.
data TextEdge = TextEdge {node1 :: Text, node2 :: Text}