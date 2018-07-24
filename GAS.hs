{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M

import Data.Char
import Data.List

{-
    Position on the gameboard(line, column)
-}
type Position = (Int, Int)

{-
    Color of the Squares/Circles
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Squares/arrows orientation
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"
{-
  An object on the boardgame(Square/Circle/Arrow)
-}
data Object = Square Heading Color | Circle Color| Arrow Heading
    deriving (Eq, Ord)

{-
  String representation of an object
-}

instance Show Object where
    show (Square heading color) = [head $ show (color)] ++ show (heading)
    show (Circle color) = [toLower $ head $ show (color)]
    show (Arrow heading) = show (heading)

maxCol :: [Position] -> Int
maxCol posList = maximum $ map snd posList

minCol :: [Position] -> Int
minCol posList = minimum $ map snd posList

maxLine :: [Position] -> Int
maxLine posList = maximum $ map fst posList

minLine :: [Position] -> Int
minLine posList = minimum $ map fst posList

allPositions :: M.Map Position [Object] -> [Position]
allPositions levelMap = [(i,j) | i <- [xm..xM], j<-[ym..yM]]
                        where xm = minLine $ M.keys levelMap
                              ym = minCol $ M.keys levelMap
                              xM = maxLine $ M.keys levelMap
                              yM = maxCol $ M.keys levelMap

delimiter :: M.Map Position [Object] -> Position -> String
delimiter levelMap pos
            | snd pos == (maxCol $ M.keys levelMap) &&
              fst pos /= (maxLine $ M.keys levelMap) = "\n"
            | snd pos == (maxCol $ M.keys levelMap) &&
              fst pos == (maxLine $ M.keys levelMap) = ""
            | otherwise = "|"

showObj :: Maybe [Object] -> String
showObj (Nothing) = "   "
showObj (Just []) = "   "
showObj (Just [Square h c]) = show (Square h c) ++ " "
showObj (Just [Circle c]) = "  " ++ show (Circle c)
showObj (Just [Arrow h]) = "  " ++  show (Arrow h)
showObj (Just [Square h c1, Circle c2]) = show (Square h c1) ++ show (Circle c2)
showObj (Just [Square h1 c, Arrow h2]) = show (Square h1 c) ++ show (Arrow h2)
showObj (Just objects) = concatMap show objects

{- A game level -}
data Level =  Level (M.Map Position [Object])
    deriving (Eq, Ord)

{- String representation of a level -}
instance Show Level where
    show(Level level) = concatMap (\pos ->
                              (showObj $ M.lookup pos level) ++ delimiter level pos)
                        $ allPositions level
emptyLevel :: Level
emptyLevel = Level M.empty

addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading pos (Level levelMap) = Level $ M.insertWith (++)
                        pos [Square heading color] levelMap

addCircle :: Color -> Position -> Level -> Level
addCircle color pos (Level levelMap) = Level $ M.insert pos [Circle color] levelMap

addArrow :: Heading -> Position -> Level -> Level
addArrow heading pos (Level levelMap) = Level $ M.insert pos [Arrow heading] levelMap

nextPos :: Heading -> Position -> Position
nextPos North (x, y) = (x - 1, y)
nextPos South (x, y) = (x + 1, y)
nextPos West  (x, y) = (x, y - 1)
nextPos East  (x, y) = (x, y + 1)

isSquare :: Object -> Bool
isSquare (Square h c) = True
isSquare _ = False

isArrow :: Object -> Bool
isArrow (Arrow h) = True
isArrow _ = False

getArrow :: Position ->  M.Map Position [Object] -> Maybe Object
getArrow pos levelMap = case M.lookup pos levelMap of
                           Just x -> find isArrow x
                           Nothing -> Nothing

getSquare :: Position ->  M.Map Position [Object] -> Maybe Object
getSquare pos levelMap = case M.lookup pos levelMap of
                           Just x -> find isSquare x
                           Nothing -> Nothing

removeSquare :: Position -> M.Map Position [Object] -> M.Map Position [Object]
removeSquare pos levelMap = case M.lookup pos levelMap of
                              Just [(Square h c)] -> M.delete pos levelMap
                              Just ((Square h c) : xs) -> M.insert pos xs levelMap
                              Just _ -> levelMap
                              Nothing -> levelMap

newHeading :: Maybe Object ->  Object -> Object
newHeading (Just (Arrow h)) (Square _ c) = Square h c
newHeading (Just _) s  = s
newHeading Nothing s  = s

getHeading :: Object -> Maybe Heading -> Heading
getHeading _ (Just h) = h
getHeading (Square h _) Nothing = h
getHeading _ _ = undefined


move' :: Maybe Heading
     -> Position
     -> Level     -- Initial level
     -> Level     -- Final level
move' heading pos (Level levelMap) = case getSquare pos levelMap of
          Nothing -> (Level levelMap)
          Just square ->
              let sh = getHeading square heading
                  next       = nextPos sh pos
                  arrow      = getArrow next levelMap
                  newSquare  = newHeading arrow square
                  (Level newLevel) = move' (Just sh) next (Level levelMap)
              in Level $ M.insertWith (++) next [newSquare] $ removeSquare pos newLevel

noSquares :: [[Object]] -> Int
noSquares elems = length $ filter (isSquare . head)  elems

{- Moves a square at the position given -}
move :: Position
     -> Level
     -> Level
move = (move' Nothing)


isValid :: [Object] -> Bool
isValid [Square h c1, Circle c2] = c1 == c2
isValid _ = False

manhattan :: Position -> Position -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

goodCircle :: Color -> Object -> Bool
goodCircle c' (Circle c) = c == c'
goodCircle c' _ = False

matchingCircle :: Object -> M.Map Position [Object] -> Position
matchingCircle (Square _ c) levelMap = fst $ head $ circles
      where circles = filter (\pair -> goodCircle c $ last $ snd pair) $  M.toList levelMap

instance ProblemState Level Position where
    successors (Level levelMap) = zip positions $ map ((flip move) (Level levelMap)) positions
      where positions = map fst $ filter (\pair -> isSquare $ head $ snd pair) $  M.toList levelMap

{- If all squares have reached their matching circle, the level is completed-}
    isGoal (Level levelMap) = noSquares elems == (length $ filter isValid elems)
                                where elems = M.elems levelMap

    heuristic (Level levelMap) = foldl (+) 0 distances
      where squares   = filter (\pair -> isSquare $ head $ snd pair) $  M.toList levelMap
            distances = map (\sq -> manhattan (fst sq) (matchingCircle (head $ snd sq) levelMap)) squares
