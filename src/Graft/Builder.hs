-- | Pure, persistent Rowan-style construction and retroactive wrapping.
-- Lexing and parsing remain the caller's responsibility. Trivia is supplied
-- as ordinary token text; construction performs no grammar validation.
module Graft.Builder
  ( Builder
  , Checkpoint
  , BuilderError (..)
  , emptyBuilder
  , startNode
  , emitToken
  , emitElement
  , finishNode
  , checkpoint
  , startNodeAt
  , runBuilder
  ) where

import Data.Text (Text)
import Graft.Green

-- | A pending forest in reverse source order, its cached element count, and
-- a stack of open node kinds and starting positions. Constructors are hidden.
-- Keeping old builder values permits independent branches without mutation.
data Builder kind = Builder !Int [GreenElement kind] [(kind, Int)]
  deriving (Eq, Show)

-- | A position in the pending forest, counting nodes and tokens, not bytes.
-- Like Rowan's checkpoint, this is a positional marker, not an owned handle:
-- it records neither builder provenance nor historical validity. Use it with
-- the corresponding construction history. Closing nodes collapses positions;
-- later emission can make an old position numerically valid again.
newtype Checkpoint = Checkpoint Int
  deriving (Eq, Show)

-- | Construction failures. Failed operations leave the supplied builder
-- available unchanged. Checkpoint error fields are marker position followed
-- by current pending count or the current node's starting position.
data BuilderError
  = NoOpenNode
  | UnclosedNodes Int
  | EmptyBuilder
  | MultipleRoots Int
  | TokenRoot
  | CheckpointOutOfBounds Int Int
  | CheckpointBeforeCurrentNode Int Int
  deriving (Eq, Show)

-- | Empty builder with no open nodes or emitted elements.
emptyBuilder :: Builder kind
emptyBuilder = Builder 0 [] []

-- | Open a node at the current end of the pending forest. Constant time.
-- The node must subsequently be closed by 'finishNode'.
startNode :: kind -> Builder kind -> Builder kind
startNode kind (Builder count pending parents) =
  Builder count pending ((kind, count) : parents)

-- | Emit exact token text, including empty text or trivia. May also be used
-- before opening a root for later wrapping by 'startNodeAt'. Token construction
-- has the cost of 'greenToken'; adding it to the builder takes constant time.
emitToken :: kind -> Text -> Builder kind -> Builder kind
emitToken kind text = emitElement (Token (greenToken kind text))

-- | Append an existing green element in constant time, reusing its value and
-- cached length without rendering or traversing descendants. This can seed
-- syntax-preserving construction with untouched subtrees.
emitElement :: GreenElement kind -> Builder kind -> Builder kind
emitElement element (Builder count pending parents) =
  Builder (count + 1) (element : pending) parents

-- | Close the innermost open node. All pending elements from its starting
-- position become its children in source order; their slot is replaced with
-- one completed node. Empty nodes are allowed. Costs a traversal of the
-- immediate children, with no traversal of their descendants.
finishNode :: Builder kind -> Either BuilderError (Builder kind)
finishNode (Builder _ _ []) = Left NoOpenNode
finishNode (Builder count pending ((kind, start) : parents)) =
  let (reversedChildren, before) = splitAt (count - start) pending
      node = greenNode kind (reverse reversedChildren)
  in Right (Builder (start + 1) (Node node : before) parents)

-- | Mark the end of the pending forest in constant time. The marker can be
-- retained while emitting tokens or completed nodes, then used for wrapping.
checkpoint :: Builder kind -> Checkpoint
checkpoint (Builder count _ _) = Checkpoint count

-- | @startNodeAt marker kind builder@ opens a node at an earlier pending
-- position, including all emitted elements since the marker and any elements
-- emitted before it is closed. The marker may equal the current end (an empty
-- initial suffix), but cannot lie past it or before the current open node.
-- A top-level suffix can be wrapped when no node is open. Constant time;
-- actual assembly is deferred to 'finishNode'. Bounds are positional only.
startNodeAt :: Checkpoint -> kind -> Builder kind -> Either BuilderError (Builder kind)
startNodeAt (Checkpoint position) kind (Builder count pending parents)
  | position > count = Left (CheckpointOutOfBounds position count)
  | (_, start) : _ <- parents, position < start =
      Left (CheckpointBeforeCurrentNode position start)
  | otherwise = Right (Builder count pending ((kind, position) : parents))

-- | Validate balanced construction and return the single completed root node.
-- Fails for unclosed nodes (checked first), an empty forest, multiple roots,
-- or a lone token. This does not implicitly close nodes or mutate the builder.
-- Checking balance takes time proportional to the number of open nodes.
runBuilder :: Builder kind -> Either BuilderError (GreenNode kind)
runBuilder (Builder count pending parents) = case parents of
  _ : _ -> Left (UnclosedNodes (length parents))
  [] -> case pending of
    [] -> Left EmptyBuilder
    [Node node] -> Right node
    [Token _] -> Left TokenRoot
    _ -> Left (MultipleRoots count)
