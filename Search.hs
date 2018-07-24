{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Set as S
import Data.List

data Node s a = Node {
    parent :: Node s a,
    state :: s,
    action :: a,
    depth :: Int
  }| Nil deriving (Eq, Show)


nodeState :: Node s a -> s
nodeState node = state node

createNode ::  (ProblemState s a) => Node s a -> Int -> (a, s) -> Node s a
createNode parent depth (action, state) = Node parent state action depth

addNodes :: (ProblemState s a, Ord s) => [Node s a] -> S.Set s -> S.Set s
addNodes nodes visited = foldl (flip S.insert) visited $ map state nodes

{- Custom comparator based of heuristic result -}
comparator :: (ProblemState s a, Ord s) => (a, s) -> (a, s) -> Ordering
comparator (_ , s1) (_, s2) = compare (heuristic s1) (heuristic s2)

needHeurestic :: (ProblemState s a, Ord s) => s -> Bool -> [(a, s)]
needHeurestic st h
            | h == False = successors st
            | otherwise  = sortBy comparator $ successors st

dfs :: (ProblemState s a, Ord s) => Node s a -> S.Set s -> Int -> Bool -> [Node s a]
dfs source visited depth h
  | S.member (state source) visited == True = []
  | depth == 0 = [source]
  | otherwise = (foldl (\nodes node -> nodes ++ (dfs node (addNodes nodes visited) (depth-1) h)) [source]
          $ map (createNode source depth) $ needHeurestic (state source) h)

limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Initial state
           -> Bool        -- True if the heuristic is used
           -> Int         -- Maximum height
           -> [Node s a]  -- Nodes list
limitedDfs s h depth = dfs (createNode Nil depth (firstAction, s)) S.empty depth h
  where firstAction = fst $ head $ successors s

iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Initial State
    -> Bool             -- True is the heuristic is used
    -> (Node s a, Int)  -- (Node with the first final state,
                        --  number of unvisited states which are not final)
iterativeDeepening s h = case findIndex (isGoal . state) list of
                              Just i -> (list !! i, i)
                              Nothing -> (head list, 0)
        where list = concatMap (limitedDfs s h) [0..]


{- THe path from a node to its parent -}
extractPath :: (Eq a, Eq s) => Node s a -> [(a, s)]
extractPath node = reverse $ map (\node -> (action node, state node)) $
                  takeWhile (\node -> parent node /= Nil) $ iterate parent node


printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
