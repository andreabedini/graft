{-# LANGUAGE BangPatterns #-}

-- | Rowan-style construction and retroactive wrapping. Lexing and parsing
-- remain the caller's responsibility. Trivia is supplied as ordinary token
-- text; construction performs no grammar validation.
--
-- A parser usually runs in the 'Build' monad, where 'buildNode' brackets a
-- node so that it cannot be left open:
--
-- @
-- expression :: Build Kind ()
-- expression = do
--   start <- buildCheckpoint
--   buildToken Number "1"
--   buildNodeAt start Binary $ do
--     buildToken Whitespace " "
--     buildToken Plus "+"
--     buildToken Whitespace " "
--     buildToken Number "2"
--
-- tree :: Either BuilderError (GreenNode Kind)
-- tree = execBuild (buildNode Root expression)
-- @
--
-- 'Build' is a thin layer over the pure 'Builder' value, which remains
-- available for code that wants to keep or branch builder states.
module Graft.Builder
  ( -- * Building in a monad
    Build
  , runBuild
  , execBuild
  , buildNode
  , buildNodeAt
  , buildToken
  , buildElement
  , buildCheckpoint
  , buildStartNode
  , buildStartNodeAt
  , buildFinishNode
  , saveBuilder
  , restoreBuilder
    -- * Pure builder values
  , Builder
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
import Graft.Green.Internal (greenNodeReversed)

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
emitElement !element (Builder count pending parents) =
  Builder (count + 1) (element : pending) parents

-- | Close the innermost open node. All pending elements from its starting
-- position become its children in source order; their slot is replaced with
-- one completed node. Empty nodes are allowed. Costs a traversal of the
-- immediate children, with no traversal of their descendants.
finishNode :: Builder kind -> Either BuilderError (Builder kind)
finishNode (Builder _ _ []) = Left NoOpenNode
finishNode (Builder count pending ((kind, start) : parents)) =
  let size = count - start
      (reversedChildren, before) = splitAt size pending
      !node = greenNodeReversed kind size reversedChildren
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

-- | A construction in progress: a state monad over 'Builder' that stops at
-- the first 'BuilderError'.
newtype Build kind a = Build (Builder kind -> Either BuilderError (a, Builder kind))

instance Functor (Build kind) where
  fmap f (Build run) = Build $ \builder -> do
    (value, next) <- run builder
    pure (f value, next)

instance Applicative (Build kind) where
  pure value = Build $ \builder -> Right (value, builder)
  Build runFunction <*> Build runValue = Build $ \builder -> do
    (f, next) <- runFunction builder
    (value, final) <- runValue next
    pure (f value, final)

instance Monad (Build kind) where
  Build run >>= continue = Build $ \builder -> do
    (value, next) <- run builder
    let Build runNext = continue value
    runNext next

-- | Run a construction from 'emptyBuilder' and validate it with 'runBuilder'.
runBuild :: Build kind a -> Either BuilderError (a, GreenNode kind)
runBuild (Build run) = do
  (value, builder) <- run emptyBuilder
  root <- runBuilder builder
  pure (value, root)

-- | Run a construction and return only its root node.
execBuild :: Build kind a -> Either BuilderError (GreenNode kind)
execBuild = fmap snd . runBuild

-- | Build a node around the elements that the body emits. The node is closed
-- when the body returns, so it cannot be left open or closed twice.
buildNode :: kind -> Build kind a -> Build kind a
buildNode kind body = buildStartNode kind *> body <* buildFinishNode

-- | Build a node that starts at an earlier checkpoint, wrapping the elements
-- emitted since then together with those the body emits. Fails as
-- 'startNodeAt' does.
buildNodeAt :: Checkpoint -> kind -> Build kind a -> Build kind a
buildNodeAt marker kind body = buildStartNodeAt marker kind *> body <* buildFinishNode

-- | Emit a token, as 'emitToken'.
buildToken :: kind -> Text -> Build kind ()
buildToken kind text = modifyBuilder (emitToken kind text)

-- | Emit an existing green element, as 'emitElement'.
buildElement :: GreenElement kind -> Build kind ()
buildElement element = modifyBuilder (emitElement element)

-- | Mark the current end of the pending forest, as 'checkpoint'.
buildCheckpoint :: Build kind Checkpoint
buildCheckpoint = Build $ \builder -> Right (checkpoint builder, builder)

-- | Open a node that a later 'buildFinishNode' closes, for parsers whose
-- start and finish happen in different places. Prefer 'buildNode'.
buildStartNode :: kind -> Build kind ()
buildStartNode kind = modifyBuilder (startNode kind)

-- | Open a node at an earlier checkpoint, as 'startNodeAt'. Prefer 'buildNodeAt'.
buildStartNodeAt :: Checkpoint -> kind -> Build kind ()
buildStartNodeAt marker kind = Build $ \builder -> (,) () <$> startNodeAt marker kind builder

-- | Close the innermost open node, as 'finishNode'.
buildFinishNode :: Build kind ()
buildFinishNode = Build $ \builder -> (,) () <$> finishNode builder

-- | The current builder state. Together with 'restoreBuilder' this lets a
-- backtracking parser return to an earlier state after a failed alternative.
saveBuilder :: Build kind (Builder kind)
saveBuilder = Build $ \builder -> Right (builder, builder)

-- | Replace the current builder state with a saved one.
restoreBuilder :: Builder kind -> Build kind ()
restoreBuilder saved = Build $ \_ -> Right ((), saved)

modifyBuilder :: (Builder kind -> Builder kind) -> Build kind ()
modifyBuilder step = Build $ \builder -> let !next = step builder in Right ((), next)
