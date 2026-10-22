{-# LANGUAGE OverloadedStrings #-}

module FoldTests (foldProperties) where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Control.DeepSeq (rnf)
import Data.Functor.Identity (Identity (..))
import Data.Hashable (hash)
import qualified Data.Text as Text
import Graft
import qualified Graft.Cursor as Cursor
import qualified Graft.Green as Green
import Test.QuickCheck
import TestModel

-- Every model becomes a node by wrapping it in a root, so leaves are tested too.
rooted :: Model -> GreenNode Kind
rooted model = Green.node Root [compile model]

leaves :: Model -> [(Kind, String)]
leaves (Leaf kind text) = [(kind, text)]
leaves (Branch _ children) = concatMap leaves children

-- Rewrites the model's leaves independently of the library.
mapLeaves :: (String -> String) -> Model -> Model
mapLeaves f (Leaf kind text) = Leaf kind (f text)
mapLeaves f (Branch kind children) = Branch kind (map (mapLeaves f) children)

observe :: GreenToken Kind -> (Kind, String)
observe token = (Green.tokenKind token, Text.unpack (Green.text token))

retext :: (String -> String) -> GreenToken Kind -> GreenToken Kind
retext f token = Green.token (Green.tokenKind token) (Text.pack (f (Text.unpack (Green.text token))))

-- Changes the byte length of most tokens, including empty ones.
grow :: String -> String
grow text = text ++ "é" ++ take 1 text

-- The fold rebuilds the oracle exactly, so it sees every kind, every token's
-- text and the shape of the tree.
propFoldRebuildsModel :: Model -> Property
propFoldRebuildsModel model =
  Green.fold (\token -> uncurry Leaf (observe token)) Branch (rooted model) === Branch Root [model]

propFoldRendering :: Model -> Property
propFoldRendering model =
  let tree = rooted model
  in Green.fold Green.text (const Text.concat) tree === Green.renderNode tree

-- Taking only the first child of each node walks one spine of a DAG whose
-- full size is 2 ^ (bits - 2) tokens, so the fold must not force the rest.
propFoldIsLazy :: Property
propFoldIsLazy = once $
  case halfMax Group of
    Node top -> Green.fold Green.text (const (Text.concat . take 1)) top === "x"
    Token _ -> counterexample "halfMax is a token" False

-- The fold visits the elements that a cursor lists as descendants, in the
-- same order, for nodes and for a token on its own.
propFoldMOrder :: Model -> Property
propFoldMOrder model =
  let tree = compile model
      visited = reverse (runIdentity (Green.foldM (\seen element -> Identity (element : seen)) [] tree))
  in visited === map Cursor.green (Cursor.descendants (Cursor.fromGreen tree))

-- In Either the fold stops at the first Left, which is the first leaf with a
-- given kind in source order.
propFoldMStops :: Model -> Property
propFoldMStops model =
  let firstSpace = [text | (Space, text) <- leaves model]
      step () (Token token) | Green.tokenKind token == Space = Left (observe token)
      step () _ = Right ()
  in either (Just . snd) (const Nothing) (Green.foldM step () (compile model))
       === case firstSpace of { text : _ -> Just text; [] -> Nothing }

-- The accumulator is evaluated before every step, even by a step that
-- ignores it, so a fold cannot build a chain of thunks. The empty node at the
-- end gives the fold one more step after every token.
propFoldMForcesAccumulator :: Model -> Property
propFoldMForcesAccumulator model =
  let step :: Int -> GreenElement Kind -> Identity Int
      step _ (Token _) = Identity (error "forced")
      step _ (Node _) = Identity 0
  in not (null (leaves model)) ==>
       failsWith "forced" (runIdentity (Green.foldM step 0 (compile (Branch Root [model, Branch Group []]))))

-- The hash depends on how the nodes nest, not only on the tokens and the
-- kinds in order.
propHashKeepsNesting :: Property
propHashKeepsNesting = once $
  let tok text = Token (Green.token 'n' text)
      early = Green.node 'r' [Node (Green.node 'g' [tok "a"]), tok "b"]
      late = Green.node 'r' [Node (Green.node 'g' [tok "a", tok "b"])]
  in counterexample "same hash for different nesting" (hash early /= hash late)

-- Forcing a tree forces every kind, including a token on its own, the last
-- token of a node, and a nested node.
propForcesEveryKind :: Property
propForcesEveryKind = once $ conjoin
  [ failsWith "forced" (rnf (Token (bad "x")))
  , failsWith "forced" (rnf (Node (Green.node (Just 'r') [Token (good "a"), Token (bad "b")])))
  , failsWith "forced" (rnf (Node (Green.node (Just 'r') [Node (Green.node (Just (error "forced")) [])])))
  , rnf (Node (Green.node (Just 'r') [Token (good "a")])) === ()
  ]
  where
    good = Green.token (Just 'n')
    bad = Green.token (Just (error "forced" :: Char))

propMapIsTraverse :: Model -> Property
propMapIsTraverse model =
  let tree = rooted model
  in Green.map (retext grow) tree === runIdentity (Green.traverse (Identity . retext grow) tree)

propTraverseIdentity :: Model -> Property
propTraverseIdentity model =
  let tree = rooted model
  in runIdentity (Green.traverse Identity tree) === tree

propTraverseOrder :: Model -> Property
propTraverseOrder model =
  map observe (getConst (Green.traverse (\token -> Const [token]) (rooted model)))
    === leaves model

propTraverseRewrites :: Model -> Property
propTraverseRewrites model =
  let edited = runIdentity (Green.traverse (Identity . retext grow) (rooted model))
  in conjoin
      [ Node edited === Node (Green.node Root [compile (mapLeaves grow model)])
      , counterexample "stale cached lengths or offsets" (validCaches (Node edited))
      , Text.unpack (Green.renderNode edited) === source (mapLeaves grow model)
      ]

-- The composition law, with two effects that fail on different tokens.
propTraverseComposition :: Model -> Property
propTraverseComposition model =
  let tree = rooted model
      f token = if Green.text token == "a" then Nothing else Just (retext grow token)
      g token = if Text.length (Green.text token) > 12 then Left (observe token) else Right token
  in getCompose (Green.traverse (Compose . fmap g . f) tree)
       === fmap (Green.traverse g) (Green.traverse f tree)

foldProperties :: [(String, Property)]
foldProperties =
  [ ("foldM visits the descendants in preorder", property propFoldMOrder)
  , ("foldM stops at the first failure", property propFoldMStops)
  , ("foldM forces the accumulator before each step", property propFoldMForcesAccumulator)
  , ("hashes keep the nesting", propHashKeepsNesting)
  , ("forcing a tree forces every kind", propForcesEveryKind)
  , ("fold rebuilds the model", property propFoldRebuildsModel)
  , ("fold with concatenation renders", property propFoldRendering)
  , ("fold visits only the children it needs", propFoldIsLazy)
  , ("map is traverse with Identity", property propMapIsTraverse)
  , ("traverse with Identity is the identity", property propTraverseIdentity)
  , ("traverse visits tokens in source order", property propTraverseOrder)
  , ("traverse rewrites tokens and measures nodes again", property propTraverseRewrites)
  , ("traverse composes", property propTraverseComposition)
  ]
