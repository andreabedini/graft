{-# LANGUAGE OverloadedStrings #-}

module EditTests (editProperties) where

import Control.Exception (evaluate)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, modify', runState)
import qualified Data.ByteString as B
import Data.List (isPrefixOf, nub, sortOn, stripPrefix, unsnoc)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Graft
import qualified Graft.Cursor as Cursor
import qualified Graft.Edit as Edit
import qualified Graft.Green as Green
import qualified Graft.Range as Range
import System.Mem.StableName (makeStableName)
import Test.QuickCheck
import TestModel

childOf :: Int -> GreenNode Kind -> GreenElement Kind
childOf index node = maybe (error "missing child") id (Green.childAt index node)

cursorAt :: Path -> GreenElement Kind -> Cursor Kind
cursorAt path base = either (error . show) id (Cursor.at path (Cursor.fromGreen base))

-- A second compilation of a model, kept apart from the first by the
-- optimiser, so that the two trees are equal but not the same object.
separately :: Model -> GreenElement Kind
separately = compile
{-# NOINLINE separately #-}

-- ---------------------------------------------------------------------------
-- Rewriting

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

-- The replacements a rewrite makes, applied as a plan to the original, give
-- the rewrite's result.
propRewriteIsApply :: Model -> Property
propRewriteIsApply model = forAll (genTable model) $ \table ->
  let base = compile model
      recording ctx cursor = do
        let answer = change table ctx cursor
        case answer of
          Replace new -> modify' (Edit.replace cursor new :)
          _ -> pure ()
        pure answer
      (rewritten, collected) = runState (Edit.rewriteM recording 0 base) []
  in Edit.apply base (reverse collected) === Right rewritten

rewriteRegressions :: Property
rewriteRegressions = once $ conjoin
  [ Green.render (Edit.rewrite (\() _ -> Replace (tok "y")) () (tok "x")) === "y"
  , Green.render (Edit.rewrite (\() _ -> Replace (tok "y")) () empty) === "y"
  , Edit.rewrite (\() _ -> Descend) () empty === empty
  , Edit.rewrite (\() _ -> DescendWith ()) () (tok "x") === tok "x"
  , Green.render (Edit.rewrite grow () wide) === "aéé béécéé"
  , counterexample "offsets after a longer child" (validCaches (Edit.rewrite grow () wide))
  ]
  where
    tok = Token . Green.token Name
    empty = Node (Green.node Root [])
    wide = Node (Green.node Root [tok "a", Token (Green.token Space " "), Node (Green.node Group [tok "b", tok "c"])])
    grow () cursor = case Cursor.green cursor of
      Token token | Green.tokenKind token == Name ->
        Replace (Token (Green.token Name (Green.text token <> "éé")))
      _ -> Descend

-- ---------------------------------------------------------------------------
-- Edit plans

-- What an edit does, at the path of the element it applies to; the model of
-- a child-anchored edit is a splice of its parent.
data ModelOp = MReplace Model | MSplice Int Int [Model]
  deriving (Show)

-- A library edit on the base with its model, or Nothing for a
-- child-anchored edit on the root, which has no model.
data Planned = Planned (Edit Kind) (Maybe (Path, ModelOp))
  deriving (Show)

plannedEdit :: Planned -> Edit Kind
plannedEdit (Planned edit _) = edit

-- The edits of a plan with their positions in the list.
type ModelPlan = [(Int, Path, ModelOp)]

modelPlan :: [Planned] -> Maybe ModelPlan
modelPlan planned =
  sequence [fmap (\(path, op) -> (n, path, op)) model | (n, Planned _ model) <- zip [0 ..] planned]

genPlan :: GreenElement Kind -> Model -> Gen [Planned]
genPlan base model = do
  count <- frequency [(1, pure 0), (6, chooseInt (1, 3)), (2, pure 4)]
  vectorOf count (genEdit base model)

genEdit :: GreenElement Kind -> Model -> Gen Planned
genEdit base model = frequency
  [ (4, anywhere $ \path _ -> do
      new <- resize 3 arbitrary
      pure (Planned (Edit.replace (cursorAt path base) (compile new)) (Just (path, MReplace new))))
  , (4, anywhere $ \path selected -> do
      let width = case selected of { Branch _ children -> length children; Leaf _ _ -> 0 }
      (start, end) <- genRange width
      new <- small
      pure (Planned (Edit.splice (cursorAt path base) start end (map compile new)) (Just (path, MSplice start end new))))
  , (2, child $ \path -> do
      new <- small
      pure (Planned (Edit.insertBefore (cursorAt path base) (map compile new)) (inParent path (\i -> MSplice i i new))))
  , (2, child $ \path -> do
      new <- small
      pure (Planned (Edit.insertAfter (cursorAt path base) (map compile new)) (inParent path (\i -> MSplice (i + 1) (i + 1) new))))
  , (2, child $ \path -> pure (Planned (Edit.remove (cursorAt path base)) (inParent path (\i -> MSplice i (i + 1) []))))
  ]
  where
    small = resize 2 (listOf (resize 2 arbitrary))
    anywhere make = do
      (path, _, selected, _) <- elements (contexts model)
      make path selected
    -- A child-anchored edit mostly targets a child; the root, which has no
    -- siblings, is tried now and then.
    child make = case [path | (path@(_ : _), _, _, _) <- contexts model] of
      [] -> make []
      paths -> frequency [(9, elements paths), (1, pure [])] >>= make
    inParent path op = case unsnoc path of
      Nothing -> Nothing
      Just (parent, index) -> Just (parent, op index)

-- Mostly valid ranges and insertions, with the occasional invalid range.
genRange :: Int -> Gen (Int, Int)
genRange width = frequency
  [ (5, do start <- chooseInt (0, width); end <- chooseInt (start, width); pure (start, end))
  , (4, do start <- chooseInt (0, width); pure (start, start))
  , (1, elements [(-1, 0), (0, -1), (1, 0), (width + 1, width + 1), (0, width + 1)])
  ]

modelAt :: Path -> Model -> Model
modelAt (index : rest) (Branch _ children) = modelAt rest (children !! index)
modelAt _ model = model

-- The splices that are not valid for the base, with the error the library
-- reports for them.
invalidSplices :: Model -> ModelPlan -> [(Int, ChildEditError)]
invalidSplices model plan =
  [ (n, err) | (n, path, MSplice start end _) <- plan, Just err <- [spliceError (modelAt path model) start end] ]
  where
    spliceError target start end = case target of
      Leaf _ _ -> Just CannotEditTokenChildren
      Branch _ children
        | start < 0 -> Just (NegativeChildBoundary start)
        | end < 0 -> Just (NegativeChildBoundary end)
        | end < start -> Just (ReversedChildRange start end)
        | start > count -> Just (ChildBoundaryOutOfBounds start)
        | end > count -> Just (ChildBoundaryOutOfBounds end)
        | otherwise -> Nothing
        where
          count = length children

-- The pairs of edits the three rules reject, stated on paths and ranges: a
-- replacement with any edit at or below it; a non-empty splice with an edit
-- at or below a child it consumes; two splices of one node whose ranges
-- [a, b) and [c, d) have a < d and c < b.
conflictPairs :: ModelPlan -> [(Int, Int)]
conflictPairs plan =
  [ (n, m)
  | (n, p, op) <- plan
  , (m, q, op') <- plan
  , n < m
  , conflicts (p, op) (q, op') || conflicts (q, op') (p, op)
  ]
  where
    conflicts (p, MReplace _) (q, _) = p `isPrefixOf` q
    conflicts (p, MSplice a b _) (q, MSplice c d _) | p == q = a < d && c < b
    conflicts (p, MSplice a b _) (q, _) = case stripPrefix p q of
      Just (i : _) -> a < b && a <= i && i < b
      _ -> False

-- Two insertions at one boundary of one node, whose order is the list's.
sameBoundary :: ModelPlan -> Bool
sameBoundary plan = length boundaries /= length (nub boundaries)
  where
    boundaries = [(p, i) | (_, p, MSplice i j _) <- plan, i == j]

-- The merge, written over lists: at each node, for each boundary, the
-- insertions there in list order, then the child or what replaces it.
mergeModel :: ModelPlan -> Model -> Model
mergeModel plan = go []
  where
    go path model = case [new | (_, p, MReplace new) <- plan, p == path] of
      new : _ -> new
      [] -> case model of
        Leaf _ _ -> model
        Branch kind children ->
          let splices = sortOn (\(n, _, _, _) -> n) [(n, a, b, xs) | (n, p, MSplice a b xs) <- plan, p == path]
              count = length children
              insertions i = concat [xs | (_, a, b, xs) <- splices, a == i, b == i]
              starting i = [xs | (_, a, b, xs) <- splices, a == i, b > i]
              consumed i = or [a <= i && i < b | (_, a, b, _) <- splices]
              slot i = case starting i of
                xs : _ -> xs
                []
                  | consumed i -> []
                  | otherwise -> [go (path ++ [i]) (children !! i)]
          in Branch kind (concat [insertions i ++ (if i < count then slot i else []) | i <- [0 .. count]])

-- A valid plan gives the model's merge; an invalid one is rejected for a
-- reason the model agrees with.
propApplyModel :: Model -> Property
propApplyModel model = forAll (genPlan base model) $ \planned ->
  let edits = map plannedEdit planned
      outcome = Edit.apply base edits
      atRoot = [n | (n, Planned _ Nothing) <- zip [0 :: Int ..] planned]
  in case modelPlan planned of
       Nothing -> label "anchored at the root" $ outcome === Left (EditAtRoot (minimum atRoot))
       Just plan ->
         let invalid = invalidSplices model plan
             conflicting = conflictPairs plan
             valid = null invalid && null conflicting
             expected = mergeModel plan model
         in checkCoverage $
            cover 30 valid "valid" $
            cover 10 (not (null conflicting)) "conflicting" $
            cover 3 (not (null invalid)) "an invalid splice" $
            if valid
              then case outcome of
                Right result -> conjoin
                  [ result === compile expected
                  , Text.unpack (Green.render result) === source expected
                  , counterexample "stale cached lengths or offsets" (validCaches result)
                  ]
                Left err -> counterexample ("rejected: " ++ show err) False
              else counterexample (show outcome) $ case outcome of
                Left (InvalidSplice n err) -> (n, err) `elem` invalid
                Left (ConflictingEdits n m) -> (n, m) `elem` conflicting
                _ -> False
  where
    base = compile model

-- A valid plan with no two insertions at one boundary gives the same tree
-- in any order.
propApplyOrder :: Model -> Property
propApplyOrder model = forAll (genPlan base model) $ \planned ->
  let edits = map plannedEdit planned
      applicable = case modelPlan planned of
        Just plan -> null (invalidSplices model plan) && null (conflictPairs plan) && not (sameBoundary plan)
        Nothing -> False
  in cover 30 applicable "order-independent plan" $
     if applicable
       then forAll (shuffle edits) $ \shuffled -> Edit.apply base shuffled === Edit.apply base edits
       else property True
  where
    base = compile model

-- Replace each edit's range in the original text, reading every range in
-- the original.
patch :: Text -> [TextEdit] -> Text
patch original edits = TE.decodeUtf8 (B.concat (go 0 edits))
  where
    bytes = TE.encodeUtf8 original
    go position [] = [B.drop position bytes]
    go position (TextEdit range new : rest) =
      B.take (start - position) (B.drop position bytes) : TE.encodeUtf8 new : go end rest
      where
        start = fromIntegral (Range.start range)
        end = fromIntegral (Range.end range)

-- Patching the base's text with the text edits renders the applied tree,
-- and the edits are sorted and apart, so the patch order doesn't matter.
propTextEdits :: Model -> Property
propTextEdits model = forAll (genPlan base model) $ \planned ->
  let edits = map plannedEdit planned
  in case (Edit.apply base edits, Edit.textEdits base edits) of
       (Right result, Right texts) -> conjoin
         [ patch (Green.render base) texts === Green.render result
         , counterexample ("not sorted and apart: " ++ show texts) (apart texts)
         ]
       (Left err, Left err') -> err === err'
       (outcome, texts) -> counterexample (show (outcome, texts)) False
  where
    base = compile model
    apart texts = and (zipWith (\a b -> Range.end (editRange a) < Range.start (editRange b)) texts (drop 1 texts))

-- An empty plan returns the base, and a sibling off every edit's path is
-- reused, not copied.
propApplyShares :: Model -> Model -> Property
propApplyShares left right = ioProperty $ do
  tree <- evaluate (compile (Branch Root [left, right]))
  same <- makeStableName tree
  unchanged <- evaluate (either (error . show) id (Edit.apply tree [])) >>= makeStableName
  let first = cursorAt [0] tree
  edited <- evaluate (either (error . show) id (Edit.apply tree [Edit.replace first (compile (Leaf Name "new"))]))
  case (tree, edited) of
    (Node before, Node after) -> do
      untouched <- evaluate (childOf 1 before) >>= makeStableName
      kept <- evaluate (childOf 1 after) >>= makeStableName
      pure $ conjoin
        [ counterexample "an empty plan copied the tree" (unchanged == same)
        , counterexample "the untouched sibling was copied" (untouched == kept)
        ]
    _ -> pure (counterexample "the roots are not nodes" False)

-- A cursor from an equal tree built separately is accepted, with the same
-- result; a cursor from a tree that differs anywhere, here above an equal
-- target, is rejected by its position in the list.
propCompatibility :: Model -> Property
propCompatibility model = forAll (genPlan twin model) $ \planned ->
  let edits = map plannedEdit planned
      wrapped = compile (Branch Group [model])
      fromOther = [Edit.replace (cursorAt [0] wrapped) (compile (Leaf Name "x"))]
      anchoredAtRoot = or [True | Planned _ Nothing <- planned]
  in conjoin
       [ counterexample "an equal tree built separately" (Edit.apply base edits === Edit.apply twin edits)
       , counterexample "a different tree" (Edit.apply base fromOther === Left (EditFromAnotherTree 0))
       , counterexample "a different tree among edits" $
           anchoredAtRoot || Edit.apply base (edits ++ fromOther) == Left (EditFromAnotherTree (length edits))
       ]
  where
    base = compile model
    twin = separately model

planRegressions :: Property
planRegressions = once $ conjoin
  [ -- insertions at the ends of a splice are placed by the merge; inside, they conflict
    rendered [Edit.splice root 0 2 [tok "X"], Edit.insertBefore a [tok "I"]] === Right "IXbc"
  , rendered [Edit.splice root 0 2 [tok "X"], Edit.insertAfter sp [tok "J"]] === Right "XJbc"
  , rendered [Edit.splice root 0 2 [tok "X"], Edit.insertAfter a [tok "K"]] === Left (ConflictingEdits 0 1)
    -- insertions at one boundary keep the order of the list
  , rendered [Edit.insertBefore sp [tok "P"], Edit.insertBefore sp [tok "Q"]] === Right "aPQ bc"
  , rendered [Edit.insertBefore sp [tok "Q"], Edit.insertBefore sp [tok "P"]] === Right "aQP bc"
    -- a removed child can't also be replaced; two replacements of one element conflict
  , rendered [Edit.remove a, Edit.replace a (tok "Z")] === Left (ConflictingEdits 0 1)
  , rendered [Edit.replace a (tok "Z"), Edit.replace a (tok "Z")] === Left (ConflictingEdits 0 1)
  , rendered [Edit.replace group (tok "Z"), Edit.remove b] === Left (ConflictingEdits 0 1)
    -- the root has no siblings; a token has no children; positions are checked
  , rendered [Edit.remove root] === Left (EditAtRoot 0)
  , rendered [Edit.replace b (tok "B"), Edit.insertAfter root []] === Left (EditAtRoot 1)
  , rendered [Edit.splice a 0 0 []] === Left (InvalidSplice 0 CannotEditTokenChildren)
  , rendered [Edit.splice root 3 5 []] === Left (InvalidSplice 0 (ChildBoundaryOutOfBounds 5))
  , rendered [Edit.splice root 3 3 [tok "E"]] === Right "a bcE"
    -- the whole tree can be replaced
  , rendered [Edit.replace root (tok "r")] === Right "r"
  , Edit.textEdits wide [Edit.replace root (tok "r")] === Right [TextEdit (Range.fromLength 0 4) "r"]
    -- adjacent edits are merged, and so are edits at one offset through an empty element
  , Edit.textEdits wide [Edit.replace a (tok "X"), Edit.replace sp (tok "Y")] === Right [TextEdit (Range.fromLength 0 2) "XY"]
  , (Green.render <$> Edit.apply narrow [Edit.insertAfter e [tok "X"], Edit.insertBefore e [tok "Y"]]) === Right "aYXb"
  , Edit.textEdits narrow [Edit.insertAfter e [tok "X"], Edit.insertBefore e [tok "Y"]] === Right [TextEdit (Range.fromLength 1 0) "YX"]
  , Edit.textEdits wide [] === Right []
  ]
  where
    tok = Token . Green.token Name
    wide = Node (Green.node Root [tok "a", Token (Green.token Space " "), Node (Green.node Group [tok "b", tok "c"])])
    root = Cursor.fromGreen wide
    a = cursorAt [0] wide
    sp = cursorAt [1] wide
    group = cursorAt [2] wide
    b = cursorAt [2, 0] wide
    narrow = Node (Green.node Root [tok "a", tok "", tok "b"])
    e = cursorAt [1] narrow
    rendered edits = Green.render <$> Edit.apply wide edits

editProperties :: [(String, Property)]
editProperties =
  [ ("rewrite gives the model's tree with valid caches", property propRewriteModel)
  , ("rewriteM visits the model's elements in preorder", property propRewriteVisits)
  , ("rewriteM stops at the first failure", property propRewriteStops)
  , ("a rewrite that replaces nothing returns its argument", property propRewriteShares)
  , ("a rewrite reuses untouched siblings", property propRewriteSharesSiblings)
  , ("a rewrite is the plan of its replacements", property propRewriteIsApply)
  , ("rewrite regressions: token and empty roots, longer children", rewriteRegressions)
  , ("apply gives the model's merge or the model's rejection", property propApplyModel)
  , ("apply is order-independent but for insertions at one boundary", property propApplyOrder)
  , ("text edits patch the source to the applied tree", property propTextEdits)
  , ("apply shares the base and untouched siblings", property propApplyShares)
  , ("edits from an equal tree are accepted, from another rejected", property propCompatibility)
  , ("plan regressions: boundaries, conflicts, errors and text edits", planRegressions)
  ]
