module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
import Data.List.Split
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False


-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN [] = False
validateFEN (x:xs) = (validateh (x:xs) 0 0)
                     where
                     validateh::String->Int->Int->Bool
                     validateh (',':xs) f r = validateh xs (f+1) r
                     validateh ('/':xs) 5 r = validateh xs 0 (r+1)
                     validateh [] 5 5 = True
                     validateh [] _ _ = False
                     validateh ('b':xs) f r = validateh xs f r
                     validateh ('r':xs) f r = validateh xs f r
                     validateh _ _ _ = False

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard fen = map buildRow (splitOn ['/'] fen)

buildRow :: String -> [Cell]
buildRow str | last str == ',' = buildRow (str ++ "a")
             | otherwise = map buildCell (splitOn [','] str)

buildCell :: String -> Cell
buildCell ('r':xs) = Stack (cellMaker ('r':xs))
buildCell ('b':xs) = Stack (cellMaker ('b':xs))
buildCell _ = Empty

cellMaker :: String -> [Player]
cellMaker [] = []
cellMaker ('r':xs) = Red : cellMaker xs
cellMaker ('b':xs) = Blue : cellMaker xs

-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

path :: Pos -> Dir -> Int -> [Pos]
path (Pos posc posr) _ steps | steps < 0 =  []
path (Pos posc posr) North steps | posr >= 6 = (Pos posc posr) : path (Pos posc (posr-1)) South (steps-1)
                                 | otherwise = (Pos posc posr) : path (Pos posc (posr+1)) North (steps-1)
path (Pos posc posr) South steps | posr <= 1 = (Pos posc posr) : path (Pos posc (posr+1)) North (steps-1)
                                 | otherwise = (Pos posc posr) : path (Pos posc (posr-1)) South (steps-1)
path (Pos posc posr) West steps  | posc <= 'a'= (Pos posc posr) : path (Pos (succ posc) posr) East (steps-1)
                                 | otherwise = (Pos posc posr) : path (Pos (pred posc) posr) West (steps-1)
path (Pos posc posr) East steps  | posc >= 'f'= (Pos posc posr) : path (Pos (pred posc) posr) West (steps-1)
                                 | otherwise = (Pos posc posr) : path (Pos (succ posc) posr) East (steps-1)
path (Pos posc posr) NorthWest steps | posc == 'a' && posr==6 = (Pos posc posr) : path (Pos (succ posc) (posr-1)) SouthEast (steps-1)
                                     | posc > 'a' && posr >=6 =  (Pos posc posr) : path (Pos (pred posc) (posr-1)) SouthWest (steps-1)
                                     | posc <= 'a' && posr<6 = (Pos posc posr) : path (Pos (succ posc) (posr+1)) NorthEast (steps-1)
                                     | otherwise = (Pos posc posr) : path (Pos (pred posc) (posr+1)) NorthWest (steps-1)
path (Pos posc posr) NorthEast steps | posc =='f' && posr ==6 = (Pos posc posr) : path (Pos (pred posc) (posr-1)) SouthWest (steps-1)
                                     | posc < 'f' && posr >=6 = (Pos posc posr) : path (Pos (succ posc) (posr-1)) SouthEast (steps-1)
                                     | posc >= 'f' && posr<6 = (Pos posc posr) : path (Pos (pred posc) (posr+1)) NorthWest (steps-1)
                                     | otherwise = (Pos posc posr) : path (Pos (succ posc) (posr+1)) NorthEast (steps-1)
path (Pos posc posr) SouthEast steps | posc == 'f' && posr ==1 =  (Pos posc posr) : path (Pos (pred posc) (posr+1)) NorthWest (steps-1)
                                     | posc >= 'f' && posr >1 = (Pos posc posr) : path (Pos (pred posc) (posr-1)) SouthWest (steps-1)
                                     | posc <'f' && posr <=1 = (Pos posc posr) : path (Pos (succ posc) (posr+1)) NorthEast (steps-1)
                                     | otherwise = (Pos posc posr) : path (Pos (succ posc) (posr-1)) SouthEast (steps-1)
path (Pos posc posr) SouthWest steps | posc =='a'&& posr ==1 = (Pos posc posr) : path (Pos (succ posc) (posr+1)) NorthEast (steps-1)
                                     | posc <='a' && posr >1 = (Pos posc posr) : path (Pos (succ posc) (posr-1)) SouthEast (steps-1)
                                     | posc >'a' && posr <=1 = (Pos posc posr) : path (Pos (pred posc) (posr+1)) NorthWest (steps-1)
                                     | otherwise = (Pos posc posr) : path (Pos (pred posc) (posr-1)) SouthWest (steps-1)