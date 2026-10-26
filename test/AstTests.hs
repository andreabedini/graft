{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AstTests (astProperties) where

import Data.List (find, inits)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Graft
import qualified Graft.Ast as Ast
import qualified Graft.Cursor as Cursor
import qualified Graft.Green as Green
import qualified Graft.Range as Range
import Test.QuickCheck
import TestModel

newtype GroupView = GroupView (Cursor Kind)
instance AstView GroupView where
  type AstViewKind GroupView = Kind
  fromCursor = Ast.matchNode (== Group) GroupView
  toCursor (GroupView cursor) = cursor
instance AstNode GroupView

newtype AnyNode = AnyNode (Cursor Kind)
instance AstView AnyNode where
  type AstViewKind AnyNode = Kind
  fromCursor = Ast.matchNode (const True) AnyNode
  toCursor (AnyNode cursor) = cursor
instance AstNode AnyNode

newtype NameView = NameView (Cursor Kind)
instance AstView NameView where
  type AstViewKind NameView = Kind
  fromCursor = Ast.matchToken (== Name) NameView
  toCursor (NameView cursor) = cursor
instance AstToken NameView

newtype AnyToken = AnyToken (Cursor Kind)
instance AstView AnyToken where
  type AstViewKind AnyToken = Kind
  fromCursor = Ast.matchToken (const True) AnyToken
  toCursor (AnyToken cursor) = cursor
instance AstToken AnyToken

type Observation = (Path, TextRange, GreenElement Kind, GreenElement Kind)

observe :: Cursor Kind -> Observation
observe cursor = (Cursor.path cursor, Cursor.range cursor, Cursor.green cursor,
  Cursor.green (Cursor.root cursor))

oracle :: Model -> Path -> Observation
oracle model path = case find (\(candidate, _, _, _) -> candidate == path) (contexts model) of
  Just (_, prefix, selected, _) ->
    (path, Range.fromLength (byteLength prefix) (byteLength (source selected)), compile selected, compile model)
  Nothing -> error "invalid oracle path"

propCasting :: Model -> Property
propCasting model = conjoin
  [ case Cursor.at path (Cursor.fromGreen (compile model)) of
      Left err -> counterexample (show err) False
      Right cursor ->
        let expected = oracle model path
            group = case selected of Branch Group _ -> Just expected; _ -> Nothing
            name = case selected of Leaf Name _ -> Just expected; _ -> Nothing
            node = case selected of Branch _ _ -> Just expected; _ -> Nothing
            token = case selected of Leaf _ _ -> Just expected; _ -> Nothing
        in conjoin
          [ fmap (observe . toCursor) (fromCursor cursor :: Maybe GroupView) === group
          , fmap (observe . toCursor) (fromCursor cursor :: Maybe NameView) === name
          , fmap (observe . toCursor) (fromCursor cursor :: Maybe AnyNode) === node
          , fmap (observe . toCursor) (fromCursor cursor :: Maybe AnyToken) === token
          , case fromCursor cursor :: Maybe GroupView of
              Nothing -> property True
              Just view -> fmap (observe . toCursor) (fromCursor (toCursor view) :: Maybe GroupView) === group
          , case fromCursor cursor :: Maybe NameView of
              Nothing -> property True
              Just view -> fmap (observe . toCursor) (fromCursor (toCursor view) :: Maybe NameView) === name
          ]
  | (path, _, selected, _) <- contexts model
  ]

propFields :: Model -> Property
propFields model = conjoin
  [ case Cursor.at path (Cursor.fromGreen (compile model)) of
      Left err -> counterexample (show err) False
      Right cursor ->
        let items = case selected of Branch _ xs -> xs; Leaf _ _ -> []
            nodes = [oracle model (path ++ [index]) | (index, Branch _ _) <- zip [0 ..] items]
            groups = [oracle model (path ++ [index]) | (index, Branch Group _) <- zip [0 ..] items]
            leaves = [oracle model (path ++ [index]) | (index, Leaf _ _) <- zip [0 ..] items]
            names = [oracle model (path ++ [index]) | (index, Leaf Name _) <- zip [0 ..] items]
        in conjoin
          [ map (observe . toCursor) (Ast.children cursor :: [AnyNode]) === nodes
          , map (observe . toCursor) (Ast.children cursor :: [GroupView]) === groups
          , fmap (observe . toCursor) (Ast.child cursor :: Maybe GroupView) === listToMaybe groups
          , map (observe . toCursor) (Ast.tokens cursor :: [AnyToken]) === leaves
          , map (observe . toCursor) (Ast.tokens cursor :: [NameView]) === names
          , fmap (observe . toCursor) (Ast.token cursor :: Maybe NameView) === listToMaybe names
          ]
  | (path, _, selected, _) <- contexts model
  ]

propParents :: Model -> Property
propParents model = conjoin
  [ case Cursor.at path (Cursor.fromGreen (compile model)) of
      Left err -> counterexample (show err) False
      Right cursor ->
        let above = reverse (take (length path) (inits path))
            groups = [oracle model p | p <- above
                     , (_, _, Branch Group _, _) <- filter (\(candidate, _, _, _) -> candidate == p) (contexts model)]
            immediate = listToMaybe above
            parentGroup = case immediate of
              Just p | p `elem` map (\(candidate, _, _, _) -> candidate) groups -> Just (oracle model p)
              _ -> Nothing
        in conjoin
          [ fmap (observe . toCursor) (Ast.parent cursor :: Maybe AnyNode) === fmap (oracle model) immediate
          , fmap (observe . toCursor) (Ast.parent cursor :: Maybe GroupView) === parentGroup
          , map (observe . toCursor) (Ast.ancestors cursor :: [AnyNode]) === map (oracle model) above
          , map (observe . toCursor) (Ast.ancestors cursor :: [GroupView]) === groups
          ]
  | (path, _, _, _) <- contexts model
  ]

modelReplace :: Path -> Model -> Model -> Model
modelReplace [] replacement _ = replacement
modelReplace (index : rest) replacement (Branch kind items) =
  Branch kind (take index items ++ [modelReplace rest replacement (items !! index)] ++ drop (index + 1) items)
modelReplace _ _ (Leaf _ _) = error "invalid oracle path"

matches :: Model -> GreenElement Kind -> Property
matches expected edited = conjoin
  [ edited === compile expected
  , Text.unpack (Green.render edited) === source expected
  , counterexample "cache invariant" (validCaches edited)
  , conjoin
      [ fmap observe (Cursor.at path (Cursor.fromGreen edited)) === Right (oracle expected path)
      | (path, _, _, _) <- contexts expected
      ]
  ]

propTokenEdit :: Model -> Property
propTokenEdit subtree = forAll genText $ \newText ->
  let model = Branch Root [Leaf Name "λ", subtree, Leaf Name "tail"]
      root = compile model
  in forAll (elements [context | context@(_, _, Leaf Name _, _) <- contexts model]) $
    \(path, prefix, selected, suffix) -> case Cursor.at path (Cursor.fromGreen root) of
      Left err -> counterexample (show err) False
      Right cursor -> case fromCursor cursor :: Maybe NameView of
        Nothing -> counterexample "name view failed" False
        Just view ->
          let edited = Cursor.tree (Cursor.replace (compile (Leaf Name newText)) (toCursor view))
          in conjoin
            [ matches (modelReplace path (Leaf Name newText) model) edited
            , Text.unpack (Green.render edited) === prefix ++ newText ++ suffix
            , observe (toCursor view) === oracle model path
            , Text.unpack (Green.render (Cursor.green (toCursor view))) === source selected
            ]

propNodeEdit :: Model -> Model -> Property
propNodeEdit subtree replacement =
  let model = Branch Root [subtree]
      root = compile model
  in forAll (elements [(path, kind, items) | (path, _, Branch kind items, _) <- contexts model]) $
    \(path, kind, items) -> forAll (chooseInt (0, length items)) $ \index ->
      case Cursor.at path (Cursor.fromGreen root) of
        Left err -> counterexample (show err) False
        Right cursor -> case fromCursor cursor :: Maybe AnyNode of
          Nothing -> counterexample "node view failed" False
          Just view -> case Cursor.tree <$> Cursor.insertChild index (compile replacement) (toCursor view) of
            Left err -> counterexample (show err) False
            Right edited -> conjoin
              [ matches (modelReplace path (Branch kind (take index items ++ [replacement] ++ drop index items)) model) edited
              , observe (toCursor view) === oracle model path
              ]

-- Typed casts from every token of a walk that renames as it goes, where the
-- ancestors hold edits that are not built yet, agree with a fresh cursor on
-- the edited tree.
propEditedCasts :: Model -> Property
propEditedCasts start = forAll genText $ \newText ->
  case Cursor.firstToken (Cursor.fromGreen (compile start)) of
    Nothing -> property True
    Just first -> walk newText first start
  where
    walk newText cursor model =
      let path = Cursor.path cursor
          (renamed, model')
            | Cursor.kind cursor == Name =
                (Cursor.replace (compile (Leaf Name newText)) cursor, modelReplace path (Leaf Name newText) model)
            | otherwise = (cursor, model)
      in case Cursor.at path (Cursor.fromGreen (compile model')) of
        Left err -> counterexample (show err) False
        Right fresh -> conjoin
          [ casts renamed === casts fresh
          , case Cursor.nextToken renamed of
              Nothing -> Cursor.tree renamed === compile model'
              Just next -> walk newText next model'
          ]
    casts cursor =
      ( fmap (observe . toCursor) (fromCursor cursor :: Maybe NameView)
      , map (observe . toCursor) (Ast.ancestors cursor :: [AnyNode])
      , map (observe . toCursor) (Ast.ancestors cursor :: [GroupView])
      , fmap (\above -> ( map (observe . toCursor) (Ast.children above :: [AnyNode])
                        , map (observe . toCursor) (Ast.tokens above :: [AnyToken])))
          (Cursor.parent cursor)
      )

-- These predicates fail if first-match queries force a later cast.
newtype StopNode = StopNode (Cursor Kind)
instance AstView StopNode where
  type AstViewKind StopNode = Kind
  fromCursor = Ast.matchNode (\kind -> if kind == Comment then error "later node cast forced" else kind == Group) StopNode
  toCursor (StopNode cursor) = cursor
instance AstNode StopNode

newtype StopToken = StopToken (Cursor Kind)
instance AstView StopToken where
  type AstViewKind StopToken = Kind
  fromCursor = Ast.matchToken (\kind -> if kind == Space then error "later token cast forced" else kind == Name) StopToken
  toCursor (StopToken cursor) = cursor
instance AstToken StopToken

regressions :: Property
regressions = once $ conjoin
  [ fmap (Cursor.path . toCursor) (fromCursor emptyGroup :: Maybe GroupView) === Just []
  , fmap (Cursor.path . toCursor) (Ast.token emptyGroup :: Maybe NameView) === Nothing
  , fmap (Cursor.path . toCursor) (fromCursor (Cursor.fromGreen (tok "")) :: Maybe NameView) === Just []
  , fmap (Cursor.path . toCursor) (fromCursor (Cursor.fromGreen (Node (Green.node Name []))) :: Maybe NameView) === Nothing
  , fmap (Cursor.path . toCursor) (fromCursor (Cursor.fromGreen (Token (Green.token Group "bad"))) :: Maybe GroupView) === Nothing
  , map (Cursor.path . toCursor) (Ast.children sharedRoot :: [GroupView]) === [[0], [1]]
  , map (Cursor.range . toCursor) (Ast.children sharedRoot :: [GroupView])
      === [Range.fromLength 0 2, Range.fromLength 2 2]
  , fmap (Cursor.path . toCursor) (Ast.token sharedRoot :: Maybe NameView) === Nothing
  , map (Cursor.path . toCursor) (Ast.tokens (Cursor.fromGreen (tok "x")) :: [NameView]) === []
  , fmap (Cursor.path . toCursor) (Ast.child lazyRoot :: Maybe StopNode) === Just [0]
  , fmap (Cursor.path . toCursor) (Ast.token lazyRoot :: Maybe StopToken) === Just [2]
  , Ast.matchNode (error "token must not check node predicate") (const ()) (Cursor.fromGreen (tok "x")) === Nothing
  , Ast.matchToken (error "node must not check token predicate") (const ()) emptyGroup === Nothing
  , fmap (Cursor.range . toCursor) (fromCursor hugeRoot :: Maybe GroupView)
      === Just (Range.fromLength 0 (2 ^ hugeDepth))
  , fmap (Cursor.range . toCursor) (Ast.child hugeRoot :: Maybe GroupView)
      === Just (Range.fromLength 0 (2 ^ (hugeDepth - 1)))
  ]
  where
    tok = Token . Green.token Name
    emptyGroup = Cursor.fromGreen (Node (Green.node Group []))
    shared = Node (Green.node Group [tok "λ", tok ""])
    sharedRoot = Cursor.fromGreen (Node (Green.node Root [shared, shared]))
    lazyRoot = Cursor.fromGreen (Node (Green.node Root
      [ Node (Green.node Group []), Node (Green.node Comment []), tok "x", Token (Green.token Space " ") ]))
    hugeRoot = Cursor.fromGreen (iterate (\element -> Node (Green.node Group [element, element])) (tok "x") !! hugeDepth)

astProperties :: [(String, Property)]
astProperties =
  [ ("typed casts retain occurrences and round-trip syntax", property propCasting)
  , ("typed immediate fields and source order", property propFields)
  , ("typed parents and ancestors", property propParents)
  , ("exact-preserving edits through typed tokens", property propTokenEdit)
  , ("persistent child edits through typed nodes", property propNodeEdit)
  , ("typed casts during a renaming walk", property propEditedCasts)
  , ("typed malformed, shared-occurrence, lazy-query and large-range regressions", regressions)
  ]
