{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AstTests (astProperties) where

import Data.List (find, inits)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Graft
import Test.QuickCheck
import TestModel

newtype GroupView = GroupView (Cursor Kind)
instance AstNode GroupView where
  type AstNodeKind GroupView = Kind
  castNode = matchNode (== Group) GroupView
  nodeSyntax (GroupView cursor) = cursor

newtype AnyNode = AnyNode (Cursor Kind)
instance AstNode AnyNode where
  type AstNodeKind AnyNode = Kind
  castNode = matchNode (const True) AnyNode
  nodeSyntax (AnyNode cursor) = cursor

newtype NameView = NameView (Cursor Kind)
instance AstToken NameView where
  type AstTokenKind NameView = Kind
  castToken = matchToken (== Name) NameView
  tokenSyntax (NameView cursor) = cursor

newtype AnyToken = AnyToken (Cursor Kind)
instance AstToken AnyToken where
  type AstTokenKind AnyToken = Kind
  castToken = matchToken (const True) AnyToken
  tokenSyntax (AnyToken cursor) = cursor

type Observation = (Path, TextRange, GreenElement Kind, GreenElement Kind)

observe :: Cursor Kind -> Observation
observe cursor = (cursorPath cursor, cursorRange cursor, cursorGreen cursor,
  cursorGreen (cursorRoot cursor))

oracle :: Model -> Path -> Observation
oracle model path = case find (\(candidate, _, _, _) -> candidate == path) (contexts model) of
  Just (_, prefix, selected, _) ->
    (path, rangeFromLength (byteLength prefix) (byteLength (source selected)), compile selected, compile model)
  Nothing -> error "invalid oracle path"

propCasting :: Model -> Property
propCasting model = conjoin
  [ case cursorAt path (rootCursor (compile model)) of
      Left err -> counterexample (show err) False
      Right cursor ->
        let expected = oracle model path
            group = case selected of Branch Group _ -> Just expected; _ -> Nothing
            name = case selected of Leaf Name _ -> Just expected; _ -> Nothing
            node = case selected of Branch _ _ -> Just expected; _ -> Nothing
            token = case selected of Leaf _ _ -> Just expected; _ -> Nothing
        in conjoin
          [ fmap (observe . nodeSyntax) (castNode cursor :: Maybe GroupView) === group
          , fmap (observe . tokenSyntax) (castToken cursor :: Maybe NameView) === name
          , fmap (observe . nodeSyntax) (castNode cursor :: Maybe AnyNode) === node
          , fmap (observe . tokenSyntax) (castToken cursor :: Maybe AnyToken) === token
          , case castNode cursor :: Maybe GroupView of
              Nothing -> property True
              Just view -> fmap (observe . nodeSyntax) (castNode (nodeSyntax view) :: Maybe GroupView) === group
          , case castToken cursor :: Maybe NameView of
              Nothing -> property True
              Just view -> fmap (observe . tokenSyntax) (castToken (tokenSyntax view) :: Maybe NameView) === name
          ]
  | (path, _, selected, _) <- contexts model
  ]

propFields :: Model -> Property
propFields model = conjoin
  [ case cursorAt path (rootCursor (compile model)) of
      Left err -> counterexample (show err) False
      Right cursor ->
        let items = case selected of Branch _ xs -> xs; Leaf _ _ -> []
            nodes = [oracle model (path ++ [index]) | (index, Branch _ _) <- zip [0 ..] items]
            groups = [oracle model (path ++ [index]) | (index, Branch Group _) <- zip [0 ..] items]
            leaves = [oracle model (path ++ [index]) | (index, Leaf _ _) <- zip [0 ..] items]
            names = [oracle model (path ++ [index]) | (index, Leaf Name _) <- zip [0 ..] items]
        in conjoin
          [ map (observe . nodeSyntax) (astChildren cursor :: [AnyNode]) === nodes
          , map (observe . nodeSyntax) (astChildren cursor :: [GroupView]) === groups
          , fmap (observe . nodeSyntax) (astChild cursor :: Maybe GroupView) === listToMaybe groups
          , map (observe . tokenSyntax) (astTokens cursor :: [AnyToken]) === leaves
          , map (observe . tokenSyntax) (astTokens cursor :: [NameView]) === names
          , fmap (observe . tokenSyntax) (astToken cursor :: Maybe NameView) === listToMaybe names
          ]
  | (path, _, selected, _) <- contexts model
  ]

propParents :: Model -> Property
propParents model = conjoin
  [ case cursorAt path (rootCursor (compile model)) of
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
          [ fmap (observe . nodeSyntax) (astParent cursor :: Maybe AnyNode) === fmap (oracle model) immediate
          , fmap (observe . nodeSyntax) (astParent cursor :: Maybe GroupView) === parentGroup
          , map (observe . nodeSyntax) (astAncestors cursor :: [AnyNode]) === map (oracle model) above
          , map (observe . nodeSyntax) (astAncestors cursor :: [GroupView]) === groups
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
  , Text.unpack (renderElement edited) === source expected
  , counterexample "cache invariant" (validCaches edited)
  , conjoin
      [ fmap observe (cursorAt path (rootCursor edited)) === Right (oracle expected path)
      | (path, _, _, _) <- contexts expected
      ]
  ]

propTokenEdit :: Model -> Property
propTokenEdit subtree = forAll genText $ \newText ->
  let model = Branch Root [Leaf Name "λ", subtree, Leaf Name "tail"]
      root = compile model
  in forAll (elements [context | context@(_, _, Leaf Name _, _) <- contexts model]) $
    \(path, prefix, selected, suffix) -> case cursorAt path (rootCursor root) of
      Left err -> counterexample (show err) False
      Right cursor -> case castToken cursor :: Maybe NameView of
        Nothing -> counterexample "name view failed" False
        Just view ->
          let edited = cursorTree (replaceFocus (compile (Leaf Name newText)) (tokenSyntax view))
          in conjoin
            [ matches (modelReplace path (Leaf Name newText) model) edited
            , Text.unpack (renderElement edited) === prefix ++ newText ++ suffix
            , observe (tokenSyntax view) === oracle model path
            , Text.unpack (renderElement (cursorGreen (tokenSyntax view))) === source selected
            ]

propNodeEdit :: Model -> Model -> Property
propNodeEdit subtree replacement =
  let model = Branch Root [subtree]
      root = compile model
  in forAll (elements [(path, kind, items) | (path, _, Branch kind items, _) <- contexts model]) $
    \(path, kind, items) -> forAll (chooseInt (0, length items)) $ \index ->
      case cursorAt path (rootCursor root) of
        Left err -> counterexample (show err) False
        Right cursor -> case castNode cursor :: Maybe AnyNode of
          Nothing -> counterexample "node view failed" False
          Just view -> case cursorTree <$> insertChildFocus index (compile replacement) (nodeSyntax view) of
            Left err -> counterexample (show err) False
            Right edited -> conjoin
              [ matches (modelReplace path (Branch kind (take index items ++ [replacement] ++ drop index items)) model) edited
              , observe (nodeSyntax view) === oracle model path
              ]

-- These predicates fail if first-match queries force a later cast.
newtype StopNode = StopNode (Cursor Kind)
instance AstNode StopNode where
  type AstNodeKind StopNode = Kind
  castNode = matchNode (\kind -> if kind == Comment then error "later node cast forced" else kind == Group) StopNode
  nodeSyntax (StopNode cursor) = cursor

newtype StopToken = StopToken (Cursor Kind)
instance AstToken StopToken where
  type AstTokenKind StopToken = Kind
  castToken = matchToken (\kind -> if kind == Space then error "later token cast forced" else kind == Name) StopToken
  tokenSyntax (StopToken cursor) = cursor

regressions :: Property
regressions = once $ conjoin
  [ fmap (cursorPath . nodeSyntax) (castNode emptyGroup :: Maybe GroupView) === Just []
  , fmap (cursorPath . tokenSyntax) (astToken emptyGroup :: Maybe NameView) === Nothing
  , fmap (cursorPath . tokenSyntax) (castToken (rootCursor (tok "")) :: Maybe NameView) === Just []
  , fmap (cursorPath . tokenSyntax) (castToken (rootCursor (Node (greenNode Name []))) :: Maybe NameView) === Nothing
  , fmap (cursorPath . nodeSyntax) (castNode (rootCursor (Token (greenToken Group "bad"))) :: Maybe GroupView) === Nothing
  , map (cursorPath . nodeSyntax) (astChildren sharedRoot :: [GroupView]) === [[0], [1]]
  , map (cursorRange . nodeSyntax) (astChildren sharedRoot :: [GroupView])
      === [rangeFromLength 0 2, rangeFromLength 2 2]
  , fmap (cursorPath . tokenSyntax) (astToken sharedRoot :: Maybe NameView) === Nothing
  , map (cursorPath . tokenSyntax) (astTokens (rootCursor (tok "x")) :: [NameView]) === []
  , fmap (cursorPath . nodeSyntax) (astChild lazyRoot :: Maybe StopNode) === Just [0]
  , fmap (cursorPath . tokenSyntax) (astToken lazyRoot :: Maybe StopToken) === Just [2]
  , matchNode (error "token must not check node predicate") (const ()) (rootCursor (tok "x")) === Nothing
  , matchToken (error "node must not check token predicate") (const ()) emptyGroup === Nothing
  , fmap (cursorRange . nodeSyntax) (castNode hugeRoot :: Maybe GroupView)
      === Just (rangeFromLength 0 (2 ^ hugeDepth))
  , fmap (cursorRange . nodeSyntax) (astChild hugeRoot :: Maybe GroupView)
      === Just (rangeFromLength 0 (2 ^ (hugeDepth - 1)))
  ]
  where
    tok = Token . greenToken Name
    emptyGroup = rootCursor (Node (greenNode Group []))
    shared = Node (greenNode Group [tok "λ", tok ""])
    sharedRoot = rootCursor (Node (greenNode Root [shared, shared]))
    lazyRoot = rootCursor (Node (greenNode Root
      [ Node (greenNode Group []), Node (greenNode Comment []), tok "x", Token (greenToken Space " ") ]))
    hugeRoot = rootCursor (iterate (\element -> Node (greenNode Group [element, element])) (tok "x") !! hugeDepth)

astProperties :: [(String, Property)]
astProperties =
  [ ("typed casts retain occurrences and round-trip syntax", property propCasting)
  , ("typed immediate fields and source order", property propFields)
  , ("typed parents and ancestors", property propParents)
  , ("exact-preserving edits through typed tokens", property propTokenEdit)
  , ("persistent child edits through typed nodes", property propNodeEdit)
  , ("typed malformed, shared-occurrence, lazy-query and large-range regressions", regressions)
  ]
