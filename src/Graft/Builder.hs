{-# LANGUAGE BangPatterns #-}

-- | Building green trees from a parser's events, as a pure value. Lexing and
-- parsing are up to you; trivia is emitted as ordinary tokens, and nothing is
-- checked against a grammar.
--
-- A parser usually builds in the 'Graft.Build.Build' monad from
-- "Graft.Build", which threads the builder and its errors and brackets each
-- node. Use t'Builder' directly when you want to keep builder states
-- around, for example to branch.
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Builder as Builder
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Graft.Builder as Builder
-- >>> import qualified Graft.Green as Green
-- >>> let operand = Builder.token 'n' "1" (Builder.startNode 'r' Builder.empty)
-- >>> let Right wrapped = Builder.startNodeAt (Builder.checkpoint (Builder.startNode 'r' Builder.empty)) 'e' operand
-- >>> let Right closed = Builder.finishNode (Builder.token 'n' "2" (Builder.token 'o' "+" wrapped)) >>= Builder.finishNode
-- >>> Green.renderNode <$> Builder.finish closed
-- Right "1+2"
module Graft.Builder
  ( Builder
  , Checkpoint
  , BuilderError (..)
  , empty
  , startNode
  , startNodeAt
  , finishNode
  , token
  , element
  , checkpoint
  , finish
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Data.Text (Text)
import Graft.Green.Internal (GreenElement (..), GreenNode)
import qualified Graft.Green as Green
import qualified Graft.Green.Internal as Internal

-- | A tree under construction: the elements emitted so far and the nodes
-- still open. A builder is an ordinary immutable value, so keeping an old one
-- lets you continue from it along a different branch.
-- Pending elements are in reverse order, with their count, and each open node
-- records the position where it started.
data Builder kind = Builder !Int [GreenElement kind] [(kind, Int)]
  deriving (Eq)

-- | Shows a sequence of 'startNode' and 'element' calls that rebuilds an
-- equal builder.
instance Show kind => Show (Builder kind) where
  showsPrec precedence (Builder count pending parents) = go precedence (reverse steps)
    where
      elements = reverse pending
      opened position = [nodeKind | (nodeKind, from) <- reverse parents, from == position]
      steps = concat
        [ map Left (opened position) ++ [Right emitted | emitted <- take 1 rest]
        | (position, rest) <- zip [0 .. count] (iterate (drop 1) elements)
        ]
      go _ [] = showString "Builder.empty"
      go outer (Left nodeKind : earlier) = showParen (outer > 10) $
        showString "Builder.startNode " . showsPrec 11 nodeKind . showChar ' ' . go 11 earlier
      go outer (Right emitted : earlier) = showParen (outer > 10) $
        showString "Builder.element " . showsPrec 11 emitted . showChar ' ' . go 11 earlier

instance NFData kind => NFData (Builder kind) where
  rnf (Builder _ pending parents) = rnf pending `seq` rnf parents

-- | A position among the elements emitted so far, counted in elements, from
-- which 'startNodeAt' can open a node. As in Rowan, it is only a number: it
-- doesn't remember which builder it came from. Use it in the node where it
-- was taken, before that node closes.
--
-- 'Show' displays the position, as @Checkpoint 3@; there is no expression
-- that builds a checkpoint without a builder.
newtype Checkpoint = Checkpoint Int
  deriving (Eq, Show)

instance NFData Checkpoint where
  rnf (Checkpoint _) = ()

-- | Why construction failed. The checkpoint errors give the checkpoint's
-- position, then the number of elements or the start of the current node.
data BuilderError
  = NoOpenNode
  | UnclosedNodes !Int
  | EmptyBuilder
  | MultipleRoots !Int
  | TokenRoot
  | CheckpointOutOfBounds !Int !Int
  | CheckpointBeforeCurrentNode !Int !Int
  deriving (Eq, Show)

instance Exception BuilderError

instance NFData BuilderError where
  rnf err = err `seq` ()

-- | A builder with nothing emitted and no open nodes.
empty :: Builder kind
empty = Builder 0 [] []

-- | Open a node, which 'finishNode' closes.
startNode :: kind -> Builder kind -> Builder kind
startNode nodeKind (Builder count pending parents) =
  Builder count pending ((nodeKind, count) : parents)

-- | Emit a token. Constant time.
token :: kind -> Text -> Builder kind -> Builder kind
token tokenKind contents = element (Token (Green.token tokenKind contents))

-- | Emit an existing node or token, such as an unchanged subtree from an
-- earlier tree. Constant time; the element is shared, not copied.
element :: GreenElement kind -> Builder kind -> Builder kind
element !emitted (Builder count pending parents) =
  Builder (count + 1) (emitted : pending) parents

-- | Close the innermost open node, making everything emitted since it opened
-- into its children. Takes time proportional to the number of children.
finishNode :: Builder kind -> Either BuilderError (Builder kind)
finishNode (Builder _ _ []) = Left NoOpenNode
finishNode (Builder count pending ((nodeKind, start) : parents)) =
  case Internal.greenNodeReversed nodeKind (count - start) pending of
    (!node, before) -> Right (Builder (start + 1) (Node node : before) parents)

-- | The current position, for a later 'startNodeAt'.
checkpoint :: Builder kind -> Checkpoint
checkpoint (Builder count _ _) = Checkpoint count

-- | @startNodeAt marker kind@ opens a node that starts at an earlier
-- checkpoint, so the elements emitted since then become its first children.
-- This is how a parser wraps an operand it has already emitted once it sees
-- the operator. The checkpoint must lie within the current open node.
startNodeAt :: Checkpoint -> kind -> Builder kind -> Either BuilderError (Builder kind)
startNodeAt (Checkpoint position) nodeKind (Builder count pending parents)
  | position > count = Left (CheckpointOutOfBounds position count)
  | (_, start) : _ <- parents, position < start =
      Left (CheckpointBeforeCurrentNode position start)
  | otherwise = Right (Builder count pending ((nodeKind, position) : parents))

-- | The finished tree. Fails if a node is still open (checked first), or
-- unless exactly one node was emitted at the top level.
finish :: Builder kind -> Either BuilderError (GreenNode kind)
finish (Builder count pending parents) = case parents of
  _ : _ -> Left (UnclosedNodes (length parents))
  [] -> case pending of
    [] -> Left EmptyBuilder
    [Node node] -> Right node
    [Token _] -> Left TokenRoot
    _ -> Left (MultipleRoots count)
