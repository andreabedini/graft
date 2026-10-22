{-# LANGUAGE OverloadedStrings #-}

module FoldTests (foldProperties) where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Text as Text
import Graft
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
  [ ("fold rebuilds the model", property propFoldRebuildsModel)
  , ("fold with concatenation renders", property propFoldRendering)
  , ("fold visits only the children it needs", propFoldIsLazy)
  , ("traverse with Identity is the identity", property propTraverseIdentity)
  , ("traverse visits tokens in source order", property propTraverseOrder)
  , ("traverse rewrites tokens and measures nodes again", property propTraverseRewrites)
  , ("traverse composes", property propTraverseComposition)
  ]
