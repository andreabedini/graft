{-# LANGUAGE OverloadedStrings #-}

module EditTests (editProperties) where

import Control.Exception (evaluate)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, modify', runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Graft
import qualified Graft.Cursor as Cursor
import qualified Graft.Edit as Edit
import qualified Graft.Green as Green
import System.Mem.StableName (makeStableName)
import Test.QuickCheck
import TestModel

-- What a rewrite is told at each path. The table is generated independently
-- of any traversal and read by the library's callback and by the model's, so
-- both see the same decisions.
data Decision = ReplaceWith Model | ReplaceSame | GoOn | GoOnWith Int | Untouched
  deriving (Show)

genTable :: Model -> Gen (Map Path Decision)
genTable model = Map.fromList <$> traverse decide [path | (path, _, _, _) <- contexts model]
  where
    decide path = (,) path <$> frequency
      [ (2, ReplaceWith <$> resize 3 arbitrary)
      , (1, pure ReplaceSame)
      , (5, pure GoOn)
      , (2, GoOnWith <$> chooseInt (0, 9))
      , (2, pure Untouched)
      ]

decisionAt :: Map Path Decision -> Path -> Decision
decisionAt table path = Map.findWithDefault GoOn path table

-- A replacement carries the context it was made with, so a context passed
-- to the wrong elements shows in the result.
stamped :: Int -> Model -> Model
stamped ctx new = Branch Group [Leaf Comment (show ctx), new]

-- The library's callback.
change :: Map Path Decision -> Int -> Cursor Kind -> Change Int Kind
change table ctx cursor = case decisionAt table (Cursor.path cursor) of
  ReplaceWith new -> Replace (compile (stamped ctx new))
  ReplaceSame -> Replace (Cursor.green cursor)
  GoOn -> Descend
  GoOnWith inner -> DescendWith inner
  Untouched -> Skip

-- The model's rewrite, over the oracle tree.
rewriteModel :: Map Path Decision -> Int -> Model -> Model
rewriteModel table = go []
  where
    go path ctx model = case decisionAt table path of
      ReplaceWith new -> stamped ctx new
      ReplaceSame -> model
      Untouched -> model
      GoOn -> below ctx
      GoOnWith inner -> below inner
      where
        below inner = case model of
          Leaf _ _ -> model
          Branch kind children ->
            Branch kind [go (path ++ [i]) inner child | (i, child) <- zip [0 ..] children]

-- The paths the callback is called on, in order: nothing below a
-- replacement or a skipped element.
visits :: Map Path Decision -> Model -> [Path]
visits table = go []
  where
    go path model = path : case decisionAt table path of
      GoOn -> below
      GoOnWith _ -> below
      _ -> []
      where
        below = case model of
          Leaf _ _ -> []
          Branch _ children -> concat [go (path ++ [i]) child | (i, child) <- zip [0 ..] children]

isReplacement, isContext, isSkip :: Decision -> Bool
isReplacement decision = case decision of { ReplaceWith _ -> True; ReplaceSame -> True; _ -> False }
isContext decision = case decision of { GoOnWith _ -> True; _ -> False }
isSkip decision = case decision of { Untouched -> True; _ -> False }

-- The rewrite gives the model's tree, kinds and empty elements included, with
-- valid caches in every rebuilt node.
propRewriteModel :: Model -> Property
propRewriteModel model = forAll (genTable model) $ \table ->
  let expected = rewriteModel table 0 model
      result = Edit.rewrite (change table) 0 (compile model)
      seen = map (decisionAt table) (visits table model)
  in checkCoverage $
     cover 20 (any isReplacement seen) "a replacement" $
     cover 10 (any isContext seen) "a context" $
     cover 10 (any isSkip seen) "a skip" $
     conjoin
       [ result === compile expected
       , Text.unpack (Green.render result) === source expected
       , counterexample "stale cached lengths or offsets" (validCaches result)
       ]

logged :: Map Path Decision -> Int -> Cursor Kind -> State [Path] (Change Int Kind)
logged table ctx cursor = do
  modify' (Cursor.path cursor :)
  pure (change table ctx cursor)

-- The callback runs in preorder, on exactly the elements the model says.
propRewriteVisits :: Model -> Property
propRewriteVisits model = forAll (genTable model) $ \table ->
  let (result, visited) = runState (Edit.rewriteM (logged table) 0 (compile model)) []
  in conjoin
       [ reverse visited === visits table model
       , result === compile (rewriteModel table 0 model)
       ]

-- A failing effect stops the rewrite at once: nothing after it is visited.
propRewriteStops :: Model -> Property
propRewriteStops model = forAll (genTable model) $ \table ->
  let planned = visits table model
  in forAll (chooseInt (0, length planned - 1)) $ \k ->
    let stopAt = planned !! k
        step :: Int -> Cursor Kind -> ExceptT Path (State [Path]) (Change Int Kind)
        step ctx cursor = do
          let path = Cursor.path cursor
          lift (modify' (path :))
          if path == stopAt then throwE path else pure (change table ctx cursor)
        (outcome, visited) = runState (runExceptT (Edit.rewriteM step 0 (compile model))) []
    in conjoin
         [ outcome === Left stopAt
         , reverse visited === take (k + 1) planned
         ]

-- A rewrite that replaces nothing returns its argument, not a copy.
propRewriteShares :: Model -> Property
propRewriteShares model = ioProperty $ do
  tree <- evaluate (compile model)
  original <- makeStableName tree
  skipped <- evaluate (Edit.rewrite (\() _ -> Skip) () tree) >>= makeStableName
  descended <- evaluate (Edit.rewrite (\() _ -> Descend) () tree) >>= makeStableName
  pure $ conjoin
    [ counterexample "Skip everywhere copied the tree" (skipped == original)
    , counterexample "Descend everywhere copied the tree" (descended == original)
    ]

-- A sibling with nothing replaced below it is reused, not copied.
propRewriteSharesSiblings :: Model -> Model -> Property
propRewriteSharesSiblings left right = ioProperty $ do
  let tree = compile (Branch Root [left, right])
      table = Map.fromList [([0], ReplaceWith (Leaf Name "new"))]
  rewritten <- evaluate (Edit.rewrite (change table) 0 tree)
  case (tree, rewritten) of
    (Node before, Node after) -> do
      untouched <- evaluate (childOf 1 before) >>= makeStableName
      kept <- evaluate (childOf 1 after) >>= makeStableName
      pure (counterexample "the untouched sibling was copied" (untouched == kept))
    _ -> pure (counterexample "the roots are not nodes" False)
  where
    childOf index node = maybe (error "missing child") id (Green.childAt index node)

rewriteRegressions :: Property
rewriteRegressions = once $ conjoin
  [ Green.render (Edit.rewrite (\() _ -> Replace (tok "y")) () (tok "x")) === Text.pack "y"
  , Green.render (Edit.rewrite (\() _ -> Replace (tok "y")) () empty) === Text.pack "y"
  , Edit.rewrite (\() _ -> Descend) () empty === empty
  , Edit.rewrite (\() _ -> DescendWith ()) () (tok "x") === tok "x"
  , Green.render (Edit.rewrite grow () wide) === Text.pack "aéé béécéé"
  , counterexample "offsets after a longer child" (validCaches (Edit.rewrite grow () wide))
  ]
  where
    tok = Token . Green.token Name
    empty = Node (Green.node Root [])
    wide = Node (Green.node Root [tok "a", Token (Green.token Space " "), Node (Green.node Group [tok "b", tok "c"])])
    grow () cursor = case Cursor.green cursor of
      Token token | Green.tokenKind token == Name ->
        Replace (Token (Green.token Name (Green.text token <> Text.pack "éé")))
      _ -> Descend

editProperties :: [(String, Property)]
editProperties =
  [ ("rewrite gives the model's tree with valid caches", property propRewriteModel)
  , ("rewriteM visits the model's elements in preorder", property propRewriteVisits)
  , ("rewriteM stops at the first failure", property propRewriteStops)
  , ("a rewrite that replaces nothing returns its argument", property propRewriteShares)
  , ("a rewrite reuses untouched siblings", property propRewriteSharesSiblings)
  , ("rewrite regressions: token and empty roots, longer children", rewriteRegressions)
  ]
