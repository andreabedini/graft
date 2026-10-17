{-# LANGUAGE BangPatterns #-}

-- | Building green trees in a monad, as a parser does. 'node' brackets a
-- node, so it cannot be left open, and the first error stops the build.
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Build as Build
-- >
-- > -- 1 + 2: the operand is emitted before the parser knows it is wrapped.
-- > expression :: Build Kind ()
-- > expression = do
-- >   start <- Build.checkpoint
-- >   Build.token Number "1"
-- >   Build.nodeAt start Binary $ do
-- >     Build.token Whitespace " "
-- >     Build.token Plus "+"
-- >     Build.token Whitespace " "
-- >     Build.token Number "2"
-- >
-- > parsed :: Either BuilderError (GreenNode Kind)
-- > parsed = Build.exec (Build.node Root expression)
--
-- 'Build' is a thin layer over the pure 'Builder' value from "Graft.Builder".
module Graft.Build
  ( Build
  , run
  , exec
  , node
  , nodeAt
  , token
  , element
  , checkpoint
  , startNode
  , startNodeAt
  , finishNode
  , save
  , restore
  ) where

import Data.Text (Text)
import Graft.Builder (Builder, BuilderError, Checkpoint)
import qualified Graft.Builder as Builder
import Graft.Green.Internal (GreenElement, GreenNode)

-- | A construction in progress: a state monad over 'Builder' that stops at
-- the first 'BuilderError'.
newtype Build kind a = Build (Builder kind -> Either BuilderError (a, Builder kind))

instance Functor (Build kind) where
  fmap f (Build step) = Build $ \builder -> do
    (value, next) <- step builder
    pure (f value, next)

instance Applicative (Build kind) where
  pure value = Build $ \builder -> Right (value, builder)
  Build stepFunction <*> Build stepValue = Build $ \builder -> do
    (f, next) <- stepFunction builder
    (value, final) <- stepValue next
    pure (f value, final)

instance Monad (Build kind) where
  Build step >>= continue = Build $ \builder -> do
    (value, next) <- step builder
    let Build stepNext = continue value
    stepNext next

-- | Run a construction from 'Builder.empty' and finish it with
-- 'Builder.finish', returning its result and the tree.
run :: Build kind a -> Either BuilderError (a, GreenNode kind)
run (Build step) = do
  (value, builder) <- step Builder.empty
  built <- Builder.finish builder
  pure (value, built)

-- | Run a construction and return only the tree.
exec :: Build kind a -> Either BuilderError (GreenNode kind)
exec = fmap snd . run

-- | A node around the elements that the body emits. The node is closed when
-- the body returns, so it cannot be left open or closed twice.
node :: kind -> Build kind a -> Build kind a
node nodeKind body = startNode nodeKind *> body <* finishNode

-- | A node that starts at an earlier checkpoint, wrapping the elements
-- emitted since then together with those the body emits. Fails as
-- 'Builder.startNodeAt' does.
nodeAt :: Checkpoint -> kind -> Build kind a -> Build kind a
nodeAt marker nodeKind body = startNodeAt marker nodeKind *> body <* finishNode

-- | Emit a token.
token :: kind -> Text -> Build kind ()
token tokenKind contents = modifyBuilder (Builder.token tokenKind contents)

-- | Emit an existing node or token, such as an unchanged subtree.
element :: GreenElement kind -> Build kind ()
element emitted = modifyBuilder (Builder.element emitted)

-- | The current position, for a later 'nodeAt'.
checkpoint :: Build kind Checkpoint
checkpoint = Build $ \builder -> Right (Builder.checkpoint builder, builder)

-- | Open a node that a later 'finishNode' closes, for parsers that open and
-- close nodes in different places. Prefer 'node'.
startNode :: kind -> Build kind ()
startNode nodeKind = modifyBuilder (Builder.startNode nodeKind)

-- | Open a node at an earlier checkpoint. Prefer 'nodeAt'.
startNodeAt :: Checkpoint -> kind -> Build kind ()
startNodeAt marker nodeKind = Build $ \builder -> (,) () <$> Builder.startNodeAt marker nodeKind builder

-- | Close the innermost open node.
finishNode :: Build kind ()
finishNode = Build $ \builder -> (,) () <$> Builder.finishNode builder

-- | The current builder. Together with 'restore' this lets a backtracking
-- parser return to an earlier state after a failed alternative.
save :: Build kind (Builder kind)
save = Build $ \builder -> Right (builder, builder)

-- | Replace the current builder with a saved one.
restore :: Builder kind -> Build kind ()
restore saved = Build $ \_ -> Right ((), saved)

modifyBuilder :: (Builder kind -> Builder kind) -> Build kind ()
modifyBuilder step = Build $ \builder -> let !next = step builder in Right ((), next)
