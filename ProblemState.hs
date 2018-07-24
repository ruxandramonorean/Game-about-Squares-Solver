{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ProblemState where

class ProblemState s a | s -> a where
    {-
        For the current state return a list of pairs (action, next state)
    -}
    successors :: s -> [(a, s)]

    {-
        True if the current state is the final state.
    -}
    isGoal :: s -> Bool

    {-
        Return an estimated distance from the current state to the final state.
    -}
    heuristic :: s -> Int
    heuristic = const 0
