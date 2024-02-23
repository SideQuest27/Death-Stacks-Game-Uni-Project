module Deathstacks where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board

-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, steps :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ show tr ++ "-" ++ [tarC] ++ show tarR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

-- #############################################################################
-- #################### playerWon :: Board -> Maybe Player #####################
-- #################### - 4 Functional Points              #####################
-- #################### - 1 Coverage Point                 #####################
-- #############################################################################

playerWon :: Board -> Maybe Player
playerWon x | x/=[] = let resR = falseCounter (concat (boardChecker x Red))
                          resB = falseCounter (concat (boardChecker x Blue))
                      in if (resR<=1 && resB<=1) then finalCheck (map boilRowOwner (filter (not.null) (map eachRow x)))
                         else if resR > 1 then Just Blue
                               else Just Red
            |otherwise = Nothing

eachRow:: [Cell]->[Player]
eachRow [] = []
eachRow (Empty : xc) = eachRow xc
eachRow (Stack (x:xs): xc) = x : eachRow xc

boilRowOwner :: [Player] -> Maybe Player
boilRowOwner [x] = Just x
boilRowOwner (x:xs:[]) = if (x==xs) then Just x else Nothing
boilRowOwner (x:xs) | (x== head xs) = boilRowOwner (tail xs)
                    | otherwise = Nothing

finalCheck :: [Maybe Player] -> Maybe Player
finalCheck [x] =  x
finalCheck (x:xs:[]) = if (x==xs) then x else Nothing
finalCheck (x:xs) | (x == head xs) = finalCheck (tail xs)
                  | otherwise = Nothing

falseCounter::[Bool]->Int
falseCounter [] = 0
falseCounter (x:xs) | (x==True) = falseCounter xs
                    | (x==False) =  1 + falseCounter xs

-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 4 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves (Pos x y) cell | (stackLength cell) <= 4 = reverse (removeDuplicatesH (maybeToMove (map posToMove [pathValidator (path (Pos x y) dir steps) | dir <- [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest], steps <- [1..(stackLength cell)]])))
                             | otherwise = reverse (removeDuplicatesH (maybeToMove (map posToMove [pathValidator (path (Pos x y) dir steps) | dir <- [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest], steps <- [((stackLength cell)-4)..(stackLength cell)]])))

posToMove::[Pos]-> Maybe Move
posToMove [] = Nothing
posToMove l = Just (Move (head l) (last l) ((length (l))-1))

maybeToMove::[Maybe Move]->[Move]
maybeToMove [] = []
maybeToMove (Nothing:xs) = maybeToMove xs
maybeToMove (Just (Move x y z):xs) = (Move x y z) : (maybeToMove xs)

removeDuplicatesH::[Move]->[Move]
removeDuplicatesH l = removeDuplicates (reverse l)
                      where
                      removeDuplicates::[Move]->[Move]
                      removeDuplicates [] = []
                      removeDuplicates (r:rs) | elem r rs = removeDuplicates rs
                                              | otherwise = r:removeDuplicates rs

stackLength::Cell->Int
stackLength (Stack s) = length s
stackLength Empty = 0

pathValidator::[Pos]->[Pos]
pathValidator l |head l == last l = []
                |otherwise = l

-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

isValidMove :: Board -> Move -> Bool
isValidMove z (Move strt end s) | strt == end = False
                                | stackLength (boardTracker z strt 'a' 6) < s = False
                                | not (foldr (&&) True (concat (boardChecker z (playerExtractor (boardTracker z strt 'a' 6))))) && stackLength (boardTracker z strt 'a' 6) > s && (stackLength (boardTracker z strt 'a' 6) - s) > 4 = False
                                | not (foldr (&&) True (concat (boardChecker z (playerExtractor (boardTracker z strt 'a' 6))))) && stackLength (boardTracker z strt 'a' 6) <= 4 = False
                                | otherwise = True

boardTracker::Board->Pos->Char->Int->Cell
boardTracker (z:zs) (Pos x y)  c r | r>y = boardTracker (zs) (Pos x y) c (pred r)
                                   | otherwise = rowTracker (z) x c

rowTracker::[Cell]->Char->Char->Cell
rowTracker (z:zs) x t | t<x = rowTracker (zs) x (succ t)
                      | otherwise = z

playerExtractor::Cell->Player
playerExtractor (Stack(x:xs)) = x

boardChecker::Board->Player->[[Bool]]
boardChecker b p = map (cellChecker p) b
                   where
                   cellChecker::Player->[Cell]->[Bool]
                   cellChecker p c = map (pieceChecker p) c
                                     where
                                     pieceChecker::Player->Cell->Bool
                                     pieceChecker p (Stack (x)) |((head x)==p && (stackLength (Stack(x))) > 4) = False
                                                                | otherwise = True
                                     pieceChecker p Empty = True

-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves [] _ = []
listMoves b p = caller b p 6

caller::Board->Player->Int->[Move]
caller (x:xs) p r | not (foldr (&&) True (concat (boardChecker (x:xs) p))) = rowTrackerV2 x p 'a' r 1 ++ caller xs p (pred r)
                  | r>0 && r<=6 =  rowTrackerV2 x p 'a' r 0 ++ caller xs p (pred r)
caller _ _ 0 = []

rowTrackerV2::[Cell]->Player->Char->Int->Int->[Move]
rowTrackerV2 (Stack(x):xr) p c r 0 = if (head x)==p then (possibleMoves (Pos c r) (Stack(x))) ++ (rowTrackerV2 xr p (succ c) r 0) else rowTrackerV2 xr p (succ c) r 0
rowTrackerV2 (Stack(x):xr) p c r 1 = if ((head x)==p && (stackLength (Stack(x)))>4) then possibleMoves (Pos c r) (Stack(x)) else rowTrackerV2 xr p (succ c) r 1
rowTrackerV2 [] p c r f= []
rowTrackerV2 (_:xr) p c r f= rowTrackerV2 xr p (succ c) r f


