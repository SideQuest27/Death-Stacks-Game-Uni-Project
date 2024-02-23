-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################
import Test.Hspec
import Board
    ( buildBoard,
      path,
      validateFEN,
      Board,
      Cell(Empty,Stack),
      Player(Red, Blue),
      Pos(Pos), Dir (North,NorthEast,East,SouthEast,South,SouthWest,West,NorthWest),
      col,
      row)
import Deathstacks ( playerWon, isValidMove, listMoves, Move(Move), possibleMoves , start, target,steps, finalCheck )

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testPath
    testPlayerWon
    testPossibleMoves
    testIsValidMove
    testListMoves
    testData
    testData2

sampleBoard :: Board
sampleBoard = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
sampleBoard2 :: Board
sampleBoard2 = [[Stack [Blue,Red],Empty,Empty,Empty,Empty,Stack [Blue,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Stack [Blue,Blue,Red],Stack [Blue,Red],Empty,Stack [Blue,Red,Blue],Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Empty,Empty,Stack [Blue,Blue,Red,Red,Red,Blue],Stack [Blue,Blue]]]
sampleBoard3 :: Board
sampleBoard3 = [[Stack [Red,Red],Empty,Empty,Empty,Empty,Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Stack [Red,Blue,Red],Stack [Red,Red],Empty,Stack [Red,Red,Blue],Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Red,Blue],Stack [Red,Blue],Empty,Empty,Stack [Red,Blue,Red,Red,Red,Blue],Stack [Red,Blue]]]
sampleBoard4 :: Board
sampleBoard4 = [[Stack [Red,Red],Empty,Empty,Empty,Empty,Stack [Blue,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Stack [Blue,Blue,Red],Stack [Red,Red],Empty,Stack [Blue,Red,Blue],Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Red,Blue],Stack [Blue,Blue],Empty,Empty,Stack [Red,Blue,Red,Red,Red,Blue],Stack [Red,Blue]]]
sampleBoard5 :: Board
sampleBoard5 = [[Stack [Red,Red],Stack [Red],Stack [Red,Red],Stack [Blue,Blue,Blue,Blue,Blue,Red,Red],Stack [Red,Red],Stack [Red,Red]], [Empty,Stack [Red],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Stack [Blue],Empty,Empty,Empty,Empty], [Stack [Blue,Blue],Empty,Empty,Empty,Stack [Blue,Blue],Stack [Blue,Blue]]]
sampleBoard6 :: Board
sampleBoard6 = [[Stack [Blue,Blue,Blue,Red,Red],Stack [Red],Stack [Red,Red],Stack [Blue,Blue,Blue,Blue,Blue,Red,Red],Stack [Red,Red],Stack [Red,Red]], [Empty,Stack [Red],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Stack [Blue],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Blue],Stack [Blue,Blue]]]
sampleBoard7 :: Board
sampleBoard7 = [[Stack [Red,Blue,Red,Red,Red],Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Stack [Blue,Blue],Stack [Red,Red],Empty,Stack [Blue,Red,Blue],Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Red,Blue],Stack [Blue,Blue],Empty,Empty,Stack [Red,Blue,Red,Red,Red,Blue],Stack [Red,Blue]]]

testValidateFEN :: Spec
testValidateFEN = describe "IF Validate-Module-Board: validateFEN ..." $ do
        it "empty string is not valid" $ do
            validateFEN "" `shouldBe` (False :: Bool)
        it "valid FEN string is valid" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` (True :: Bool)
        it "FEN with missing , in not valid " $ do
           validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,bbrrrb,bb" `shouldBe` (False :: Bool)
        it "FEN with missing / in not valid " $ do
           validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,,,,,," `shouldBe` (False :: Bool)
        it "FEN string with illigal piece is not valid" $ do
           validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bba,bb,bb,bb,bb,bb" `shouldBe` (False :: Bool)

testBuildBoard :: Spec
testBuildBoard = describe "IF Validate-Module-Board: buildBoard ..." $ do
        it "build empty board" $ do
            (buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,") `shouldBe` ([[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]] :: Board)
        it "build starting board" $ do
            (buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb") `shouldBe` ([[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]:: Board)
        it "build scrambled board" $ do
            (buildBoard "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb") `shouldBe` ([[Stack [Red,Red],Empty,Empty,Empty,Empty,Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Stack [Blue,Blue,Red],Stack [Red,Red],Empty,Stack [Red,Red,Blue],Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Empty,Empty,Stack [Blue,Blue,Red,Red,Red,Blue],Stack [Blue,Blue]]] :: Board)

testPath :: Spec
testPath = describe "IF Validate-Module-Board: path ..." $ do
        it "one step north" $ do
            path (Pos 'c' 2) North 1 `shouldBe` ([(Pos 'c' 2), (Pos 'c' 3)] :: [Pos])
        it "one step south" $ do
            path (Pos 'c' 2) South 1 `shouldBe` ([(Pos 'c' 2), (Pos 'c' 1)] :: [Pos])
        it "one step east" $ do
            path (Pos 'c' 2) East 1 `shouldBe` ([(Pos 'c' 2), (Pos 'd' 2)] :: [Pos])
        it "one step west" $ do
            path (Pos 'c' 2) West 1 `shouldBe` ([(Pos 'c' 2), (Pos 'b' 2)] :: [Pos])
        it "one step north-east" $ do
            path (Pos 'c' 2) NorthEast 1 `shouldBe` ([(Pos 'c' 2), (Pos 'd' 3)] :: [Pos])
        it "one step south-east "  $ do
           path (Pos 'c' 2) SouthEast 1 `shouldBe` ([(Pos 'c' 2), (Pos 'd' 1)] :: [Pos])
        it "one step north-weast "  $ do
            path (Pos 'c' 2) NorthWest 1 `shouldBe` ([(Pos 'c' 2), (Pos 'b' 3)] :: [Pos])
        it "one step south-east "  $ do
           path (Pos 'c' 2) SouthWest 1 `shouldBe` ([(Pos 'c' 2), (Pos 'b' 1)] :: [Pos])
        it "Bouncing and coming back to the same position "  $ do
           path (Pos 'b' 2) NorthWest 10 `shouldBe` ([(Pos 'b' 2), (Pos 'a' 3),(Pos 'b' 4), (Pos 'c' 5),(Pos 'd' 6), (Pos 'e' 5),(Pos 'f' 4), (Pos 'e' 3),(Pos 'd' 2), (Pos 'c' 1),(Pos 'b' 2)] :: [Pos])
        it "Reverse Bouncing and coming back to the same position " $ do
           path (Pos 'b' 2) SouthEast 10 `shouldBe` ([(Pos 'b' 2), (Pos 'c' 1),(Pos 'd' 2), (Pos 'e' 3),(Pos 'f' 4), (Pos 'e' 5),(Pos 'd' 6), (Pos 'c' 5),(Pos 'b' 4), (Pos 'a' 3),(Pos 'b' 2)] :: [Pos])
        it "Hitting the edge north-west"  $ do
           path (Pos 'c' 4) NorthWest 3 `shouldBe` ([(Pos 'c' 4), (Pos 'b' 5),(Pos 'a' 6), (Pos 'b' 5)] :: [Pos])
        it "Hitting the edge north-east"  $ do
           path (Pos 'd' 4) NorthEast 3 `shouldBe` ([(Pos 'd' 4), (Pos 'e' 5),(Pos 'f' 6), (Pos 'e' 5)] :: [Pos])
        it "Hitting the edge south-west"  $ do
           path (Pos 'c' 3) SouthWest 3 `shouldBe` ([(Pos 'c' 3), (Pos 'b' 2),(Pos 'a' 1), (Pos 'b' 2)] :: [Pos])
        it "Hitting the edge south-east"  $ do
           path (Pos 'd' 3) SouthEast 3 `shouldBe` ([(Pos 'd' 3), (Pos 'e' 2),(Pos 'f' 1), (Pos 'e' 2)] :: [Pos])
        it "Hitting the edge north" $ do
           path (Pos 'a' 4) North 3 `shouldBe` ([(Pos 'a' 4), (Pos 'a' 5),(Pos 'a' 6), (Pos 'a' 5)] :: [Pos])
        it "Hitting the edge south" $ do
           path (Pos 'a' 3) South 3 `shouldBe` ([(Pos 'a' 3), (Pos 'a' 2),(Pos 'a' 1), (Pos 'a' 2)] :: [Pos])
        it "Hitting the edge west" $ do
           path (Pos 'c' 1) West 3 `shouldBe` ([(Pos 'c' 1), (Pos 'b' 1),(Pos 'a' 1), (Pos 'b' 1)] :: [Pos])
        it "Hitting the edge east" $ do
           path (Pos 'd' 1) East 3 `shouldBe` ([ (Pos 'd' 1),(Pos 'e' 1), (Pos 'f' 1),(Pos 'e' 1)] :: [Pos])

testPlayerWon :: Spec
testPlayerWon = describe "IF Validate-Module-Game: playerWon ..." $ do
        it "start board not finished" $ do
            playerWon sampleBoard `shouldBe` (Nothing :: Maybe Player)
        it "Blue Win's" $ do
            playerWon sampleBoard2 `shouldBe` (Just Blue :: Maybe Player)
        it "Red Win's" $ do
            playerWon sampleBoard3 `shouldBe` (Just Red :: Maybe Player)
        it "Undecided situation" $ do
            playerWon sampleBoard4 `shouldBe` (Nothing :: Maybe Player)
        it "Nothing for an empty board" $ do
            playerWon [] `shouldBe` (Nothing :: Maybe Player)
        it "Red Win's because Blue has two Too tall Stacks" $ do
            playerWon sampleBoard6 `shouldBe` (Just Red :: Maybe Player)
        it "Blue Win's because Red has two Too tall Stacks" $ do
            playerWon sampleBoard7 `shouldBe` (Just Blue :: Maybe Player)
        it "Making Sure finalCheck is fulfilled " $ do
            finalCheck [Just Red,Just Red] `shouldBe` (Just Red :: Maybe Player)
        it "Making Sure finalCheck is fulfilled V2" $ do
            finalCheck [Just Red,Just Blue,Just Red] `shouldBe` (Nothing :: Maybe Player)


testPossibleMoves :: Spec
testPossibleMoves = describe "IF Validate-Module-Game: possibleMoves ..." $ do
        it "single test" $ do
            possibleMoves (Pos 'b' 1) (Stack [Red]) `shouldMatchList` ([Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1] :: [Move])
        it "stack of size >4" $ do
            possibleMoves (Pos 'b' 1) (Stack [Red,Red,Red,Red,Red,Red]) `shouldMatchList` [Move (Pos 'b' 1) (Pos 'b' 3) 2,Move (Pos 'b' 1) (Pos 'b' 4) 3,Move (Pos 'b' 1) (Pos 'b' 5) 4,Move (Pos 'b' 1) (Pos 'b' 6) 5,Move (Pos 'b' 1) (Pos 'b' 5) 6,Move (Pos 'b' 1) (Pos 'd' 3) 2,Move (Pos 'b' 1) (Pos 'e' 4) 3,Move (Pos 'b' 1) (Pos 'f' 5) 4,Move (Pos 'b' 1) (Pos 'e' 6) 5,Move (Pos 'b' 1) (Pos 'd' 5) 6,Move (Pos 'b' 1) (Pos 'd' 1) 2,Move (Pos 'b' 1) (Pos 'e' 1) 3,Move (Pos 'b' 1) (Pos 'f' 1) 4,Move (Pos 'b' 1) (Pos 'e' 1) 5,Move (Pos 'b' 1) (Pos 'd' 1) 6,Move (Pos 'b' 1) (Pos 'c' 4) 3,Move (Pos 'b' 1) (Pos 'd' 5) 4,Move (Pos 'b' 1) (Pos 'f' 5) 6,Move (Pos 'b' 1) (Pos 'c' 1) 3,Move (Pos 'b' 1) (Pos 'd' 1) 4,Move (Pos 'b' 1) (Pos 'f' 1) 6]
        it "stack is Empty" $ do
            possibleMoves (Pos 'b' 1) (Empty) `shouldBe` []

testIsValidMove :: Spec
testIsValidMove = describe "IF Validate-Module-Game: isValidMove ..." $ do
        it "simple valid move" $ do
            let b = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 4) 2) `shouldBe` (True :: Bool)
        it "Too tall rule broken" $ do
           isValidMove sampleBoard4 (Move (Pos 'e' 1) (Pos 'e' 2) 1) `shouldBe` (False :: Bool)
        it "Valid move based on the too tall rule" $ do
           isValidMove sampleBoard4 (Move (Pos 'e' 1) (Pos 'e' 3) 2) `shouldBe` (True :: Bool)
        it "cant come back or stay on the same position" $ do
           isValidMove sampleBoard4 (Move (Pos 'e' 1) (Pos 'e' 1) 0) `shouldBe` (False :: Bool)
        it "stack length samller than the steps " $ do
           isValidMove sampleBoard4 (Move (Pos 'a' 6) (Pos 'a' 3) 3) `shouldBe` (False :: Bool)
        it "moving a smaller stack when a stack with hight > 4 exists " $ do
           isValidMove sampleBoard4 (Move (Pos 'a' 6) (Pos 'a' 5) 1) `shouldBe` (False :: Bool)
        it "Too tall rule broken with mixed color stack " $ do
            isValidMove sampleBoard5 (Move (Pos 'a' 1) (Pos 'b' 1) 1) `shouldBe` (False :: Bool)

testListMoves :: Spec
testListMoves = describe "IF Validate-Module-Game: listMoves ..." $ do
        it "red cannot move" $ do
            let board = [[Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]
                result = []
                in
                    map show (listMoves board Red) `shouldMatchList` (result :: [String])
        it "biggest stack moves" $ do
            listMoves sampleBoard4 Red `shouldBe` ([Move (Pos 'e' 1) (Pos 'e' 3) 2,Move (Pos 'e' 1) (Pos 'e' 4) 3,Move (Pos 'e' 1) (Pos 'e' 5) 4,Move (Pos 'e' 1) (Pos 'e' 6) 5,Move (Pos 'e' 1) (Pos 'e' 5) 6,Move (Pos 'e' 1) (Pos 'd' 4) 3,Move (Pos 'e' 1) (Pos 'c' 5) 4,Move (Pos 'e' 1) (Pos 'b' 6) 5,Move (Pos 'e' 1) (Pos 'a' 5) 6,Move (Pos 'e' 1) (Pos 'd' 1) 3,Move (Pos 'e' 1) (Pos 'c' 1) 4,Move (Pos 'e' 1) (Pos 'b' 1) 5,Move (Pos 'e' 1) (Pos 'a' 1) 6,Move (Pos 'e' 1) (Pos 'c' 3) 2,Move (Pos 'e' 1) (Pos 'b' 4) 3,Move (Pos 'e' 1) (Pos 'a' 5) 4,Move (Pos 'e' 1) (Pos 'c' 5) 6,Move (Pos 'e' 1) (Pos 'c' 1) 2,Move (Pos 'e' 1) (Pos 'b' 1) 3,Move (Pos 'e' 1) (Pos 'a' 1) 4,Move (Pos 'e' 1) (Pos 'c' 1) 6])
        it "all possible moves if all stacks are smaller than 5 " $ do
            listMoves sampleBoard4 Blue `shouldBe` ([Move (Pos 'f' 6) (Pos 'f' 5) 1,Move (Pos 'f' 6) (Pos 'f' 4) 2,Move (Pos 'f' 6) (Pos 'e' 5) 1,Move (Pos 'f' 6) (Pos 'd' 4) 2,Move (Pos 'f' 6) (Pos 'e' 6) 1,Move (Pos 'f' 6) (Pos 'd' 6) 2,Move (Pos 'b' 4) (Pos 'b' 5) 1,Move (Pos 'b' 4) (Pos 'b' 6) 2,Move (Pos 'b' 4) (Pos 'b' 5) 3,Move (Pos 'b' 4) (Pos 'c' 5) 1,Move (Pos 'b' 4) (Pos 'd' 6) 2,Move (Pos 'b' 4) (Pos 'e' 5) 3,Move (Pos 'b' 4) (Pos 'c' 4) 1,Move (Pos 'b' 4) (Pos 'd' 4) 2,Move (Pos 'b' 4) (Pos 'e' 4) 3,Move (Pos 'b' 4) (Pos 'c' 3) 1,Move (Pos 'b' 4) (Pos 'd' 2) 2,Move (Pos 'b' 4) (Pos 'e' 1) 3,Move (Pos 'b' 4) (Pos 'b' 3) 1,Move (Pos 'b' 4) (Pos 'b' 2) 2,Move (Pos 'b' 4) (Pos 'b' 1) 3,Move (Pos 'b' 4) (Pos 'a' 3) 1,Move (Pos 'b' 4) (Pos 'c' 1) 3,Move (Pos 'b' 4) (Pos 'a' 4) 1,Move (Pos 'b' 4) (Pos 'c' 4) 3,Move (Pos 'b' 4) (Pos 'a' 5) 1,Move (Pos 'b' 4) (Pos 'c' 5) 3,Move (Pos 'e' 4) (Pos 'e' 5) 1,Move (Pos 'e' 4) (Pos 'e' 6) 2,Move (Pos 'e' 4) (Pos 'e' 5) 3,Move (Pos 'e' 4) (Pos 'f' 5) 1,Move (Pos 'e' 4) (Pos 'd' 5) 3,Move (Pos 'e' 4) (Pos 'f' 4) 1,Move (Pos 'e' 4) (Pos 'd' 4) 3,Move (Pos 'e' 4) (Pos 'f' 3) 1,Move (Pos 'e' 4) (Pos 'e' 2) 2,Move (Pos 'e' 4) (Pos 'd' 1) 3,Move (Pos 'e' 4) (Pos 'e' 3) 1,Move (Pos 'e' 4) (Pos 'e' 1) 3,Move (Pos 'e' 4) (Pos 'd' 3) 1,Move (Pos 'e' 4) (Pos 'c' 2) 2,Move (Pos 'e' 4) (Pos 'b' 1) 3,Move (Pos 'e' 4) (Pos 'd' 4) 1,Move (Pos 'e' 4) (Pos 'c' 4) 2,Move (Pos 'e' 4) (Pos 'b' 4) 3,Move (Pos 'e' 4) (Pos 'd' 5) 1,Move (Pos 'e' 4) (Pos 'c' 6) 2,Move (Pos 'e' 4) (Pos 'b' 5) 3,Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'b' 3) 2,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'd' 3) 2,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'd' 1) 2,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1])
        it "[]->[]" $ do
            listMoves [] Red `shouldBe` []

testData :: Spec
testData =  describe "Pos,Cell" $ do
           it "returns True for equal positions" $ do
               Pos 'a' 1 == Pos 'a' 1 `shouldBe` True
           it "returns False for different positions" $ do
               Pos 'a' 1 == Pos 'b' 1 `shouldBe` False
           it "returns True for equal Red players" $ do
               Red == Red `shouldBe` True
           it "returns False for different players" $ do
               Red == Blue `shouldBe` False
           it "returns True for equal Empty cells" $ do
               Empty == Empty `shouldBe` True
           it "returns True for equal Stack cells" $ do
               Stack [Red, Blue] == Stack [Red, Blue] `shouldBe` True
           it "returns False for different Stack cells" $ do
               Stack [Red, Blue] == Stack [Blue, Red] `shouldBe` False
           it "returns False for different cells" $
               Empty == Stack [Red] `shouldBe` False
           it "returns the correct column value" $ do
               col (Pos 'a' 1) `shouldBe` 'a'
           it "returns the correct row value" $ do
               row (Pos 'a' 1) `shouldBe` 1
           it "returns the correct start position" $ do
               start (Move (Pos 'a' 1) (Pos 'b' 2) 3) `shouldBe` Pos 'a' 1
           it "returns the correct target position" $ do
             target (Move (Pos 'a' 1) (Pos 'b' 2) 1) `shouldBe` Pos 'b' 2
           it "returns the correct number of steps" $ do
             steps (Move (Pos 'a' 1) (Pos 'b' 2) 1) `shouldBe` 1


testData2 :: Spec
testData2 =  describe "Move" $ do
           it "returns True for equal moves" $ do
               Move (Pos 'a' 1) (Pos 'b' 2) 1 == Move (Pos 'a' 1) (Pos 'b' 2) 1 `shouldBe` True
           it "returns False for different moves" $ do
               Move (Pos 'a' 1) (Pos 'b' 2) 1 == Move (Pos 'a' 4) (Pos 'a' 6) 2 `shouldBe` False
           it "returns the expected string representation" $ do
               show (Move (Pos 'a' 1) (Pos 'b' 2) 1) `shouldBe` "a1-1-b2"
