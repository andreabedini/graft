module CursorTests (cursorProperties) where

import qualified Data.Text as Text
import Graft
import qualified Graft.Cursor as Cursor
import qualified Graft.Range as Range
import Test.QuickCheck
import TestModel

-- One step of a program run on a cursor and on the model.
data Step
  = Up
  | Next
  | Previous
  | First
  | Last
  | Child Int
  | Top
  | Look
  | Lookup Int
  deriving (Show)

genStep :: Gen Step
genStep = frequency
  [ (4, pure Next), (2, pure Previous), (3, pure Up), (3, pure First), (1, pure Last)
  , (1, Child <$> chooseInt (0, 4)), (1, pure Top)
  , (2, pure Look), (1, Lookup <$> chooseInt (0, 40))
  ]

shrinkStep :: Step -> [Step]
shrinkStep (Child index) = Child <$> shrink index
shrinkStep (Lookup offset) = Lookup <$> shrink offset
shrinkStep _ = []

modelAt :: Path -> Model -> Maybe Model
modelAt [] model = Just model
modelAt (index : rest) (Branch _ items)
  | index >= 0, index < length items = modelAt rest (items !! index)
modelAt _ _ = Nothing

modelKind :: Model -> Kind
modelKind (Branch kind _) = kind
modelKind (Leaf kind _) = kind

modelChildren :: Model -> [Model]
modelChildren (Branch _ items) = items
modelChildren (Leaf _ _) = []

isLeaf :: Model -> Bool
isLeaf (Leaf _ _) = True
isLeaf (Branch _ _) = False

-- The path, range and kind of an occurrence, from the model's own text.
expected :: Model -> Path -> (Path, TextRange, Kind, Bool)
expected model path = case [(prefix, selected) | (other, prefix, selected, _) <- contexts model, other == path] of
  (prefix, selected) : _ ->
    (path, Range.fromLength (byteLength prefix) (byteLength (source selected)), modelKind selected, isLeaf selected)
  [] -> error ("no occurrence at " ++ show path)

observe :: Cursor Kind -> (Path, TextRange, Kind, Bool)
observe cursor = (Cursor.path cursor, Cursor.range cursor, Cursor.kind cursor, Cursor.isToken cursor)

-- Where a step leads in the model, or Nothing where the cursor has nowhere
-- to go.
target :: Model -> Path -> Step -> Maybe Path
target model path step = case step of
  Up -> case reverse path of
    _ : above -> Just (reverse above)
    [] -> Nothing
  Next -> sibling 1
  Previous -> sibling (-1)
  First -> child 0
  Last -> child (length focusChildren - 1)
  Child index -> child index
  Top -> Just []
  _ -> Just path
  where
    focusChildren = maybe [] modelChildren (modelAt path model)
    child index
      | index >= 0, index < length focusChildren = Just (path ++ [index])
      | otherwise = Nothing
    sibling offset = case reverse path of
      index : above
        | Just above' <- modelAt (reverse above) model
        , let next = index + offset
        , next >= 0, next < length (modelChildren above') -> Just (reverse above ++ [next])
      _ -> Nothing

move :: Step -> Cursor Kind -> Maybe (Cursor Kind)
move step cursor = case step of
  Up -> Cursor.parent cursor
  Next -> Cursor.nextSibling cursor
  Previous -> Cursor.previousSibling cursor
  First -> Cursor.firstChild cursor
  Last -> Cursor.lastChild cursor
  Child index -> either (const Nothing) Just (Cursor.childAt index cursor)
  Top -> Just (Cursor.root cursor)
  _ -> Just cursor

-- Run a program on a cursor and on the model. After every step the focus
-- and its ancestors agree with the model; reading steps also compare the
-- focus, its children and offset lookups from the root; at the end the
-- cursor is still in the tree it started in.
propProgram :: Model -> Property
propProgram model = forAllShrink (listOf genStep) (shrinkList shrinkStep) $ \steps ->
  go (Cursor.fromGreen tree) [] steps
  where
    tree = compile model
    go cursor path [] =
      counterexample "tree kept" (Cursor.tree cursor === tree)
        .&&. counterexample "path kept" (Cursor.path cursor === path)
    go cursor path (step : rest) = case (move step cursor, target model path step) of
      (Nothing, Nothing) -> go cursor path rest
      (Just moved, Just path') ->
        counterexample (show step) (check moved path' step) .&&. go moved path' rest
      (moved, path') ->
        counterexample (show step ++ ": cursor " ++ show (fmap Cursor.path moved) ++ ", model " ++ show path')
          False
    check cursor path step = conjoin $
      [ observe cursor === expected model path
      , map observe (Cursor.ancestors cursor) === map (expected model) (drop 1 (reverse (inits' path)))
      ] ++ case step of
        Look ->
          [ Cursor.green cursor === maybe (error "no focus") compile (modelAt path model)
          , Text.unpack (Cursor.text cursor) === maybe "" source (modelAt path model)
          , map observe (Cursor.children cursor)
              === [expected model (path ++ [index]) | index <- [0 .. length (maybe [] modelChildren (modelAt path model)) - 1]]
          ]
        Lookup offset ->
          let fresh = Cursor.fromGreen tree
              position = fromIntegral offset
              query = Range.fromLength position 0
          in [ fmap observe (Cursor.tokenAt position (Cursor.root cursor)) === fmap observe (Cursor.tokenAt position fresh)
             , fmap observe (Cursor.covering query (Cursor.root cursor)) === fmap observe (Cursor.covering query fresh)
             ]
        _ -> []
    inits' path = [take count path | count <- [0 .. length path]]

cursorProperties :: [(String, Property)]
cursorProperties =
  [ ("cursor programs of moves and lookups against the model", property propProgram)
  ]
