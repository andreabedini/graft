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

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Data.Text (Text)
import Graft.Green
import Graft.Green.Internal (greenNodeReversed)

-- | A tree under construction: the elements emitted so far and the nodes
-- still open. A builder is an ordinary immutable value, so keeping an old one
-- lets you continue from it along a different branch.
-- Pending elements are in reverse order, with their count, and each open node
-- records the position where it started.
data Builder kind = Builder !Int [GreenElement kind] [(kind, Int)]
  deriving (Eq)

-- | Shows a sequence of 'startNode' and 'emitElement' calls that rebuilds an
-- equal builder.
instance Show kind => Show (Builder kind) where
  showsPrec precedence (Builder count pending parents) = go precedence (reverse steps)
    where
      elements = reverse pending
      opened position = [kind | (kind, start) <- reverse parents, start == position]
      steps = concat
        [ map Left (opened position) ++ [Right element | element <- take 1 rest]
        | (position, rest) <- zip [0 .. count] (iterate (drop 1) elements)
        ]
      go _ [] = showString "emptyBuilder"
      go outer (Left kind : earlier) = showParen (outer > 10) $
        showString "startNode " . showsPrec 11 kind . showChar ' ' . go 11 earlier
      go outer (Right element : earlier) = showParen (outer > 10) $
        showString "emitElement " . showsPrec 11 element . showChar ' ' . go 11 earlier

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
  | UnclosedNodes Int
  | EmptyBuilder
  | MultipleRoots Int
  | TokenRoot
  | CheckpointOutOfBounds Int Int
  | CheckpointBeforeCurrentNode Int Int
  deriving (Eq, Show)

instance Exception BuilderError

instance NFData BuilderError where
  rnf err = err `seq` ()

-- | A builder with nothing emitted and no open nodes.
emptyBuilder :: Builder kind
emptyBuilder = Builder 0 [] []

-- | Open a node, which 'finishNode' closes.
startNode :: kind -> Builder kind -> Builder kind
startNode kind (Builder count pending parents) =
  Builder count pending ((kind, count) : parents)

-- | Emit a token. Constant time.
emitToken :: kind -> Text -> Builder kind -> Builder kind
emitToken kind text = emitElement (Token (greenToken kind text))

-- | Emit an existing node or token, such as an unchanged subtree from an
-- earlier tree. Constant time; the element is shared, not copied.
emitElement :: GreenElement kind -> Builder kind -> Builder kind
emitElement !element (Builder count pending parents) =
  Builder (count + 1) (element : pending) parents

-- | Close the innermost open node, making everything emitted since it opened
-- into its children. Takes time proportional to the number of children.
finishNode :: Builder kind -> Either BuilderError (Builder kind)
finishNode (Builder _ _ []) = Left NoOpenNode
finishNode (Builder count pending ((kind, start) : parents)) =
  case greenNodeReversed kind (count - start) pending of
    (!node, before) -> Right (Builder (start + 1) (Node node : before) parents)

-- | The current position, for a later 'startNodeAt'.
checkpoint :: Builder kind -> Checkpoint
checkpoint (Builder count _ _) = Checkpoint count

-- | @startNodeAt marker kind@ opens a node that starts at an earlier
-- checkpoint, so the elements emitted since then become its first children.
-- This is how a parser wraps an operand it has already emitted once it sees
-- the operator. The checkpoint must lie within the current open node.
startNodeAt :: Checkpoint -> kind -> Builder kind -> Either BuilderError (Builder kind)
startNodeAt (Checkpoint position) kind (Builder count pending parents)
  | position > count = Left (CheckpointOutOfBounds position count)
  | (_, start) : _ <- parents, position < start =
      Left (CheckpointBeforeCurrentNode position start)
  | otherwise = Right (Builder count pending ((kind, position) : parents))

-- | The finished tree. Fails if a node is still open (checked first), or
-- unless exactly one node was emitted at the top level.
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
