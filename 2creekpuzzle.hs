module CreekPuzzle where

-- assumptions taken: 0 - on board is empty, 1 - is colored, 2 - must be empty , 3 - must remain colored
import Data.Array
import Data.List

-- where we will be solving our puzzle
type CreekBoard = [[Int]]
-- position of a cell in CreekBoard
type Position = (Int, Int)

type NumberPosition = ((Int,Int),Int)

-- user data to read CreekPuzzle description
data Creek = Empty | Creek (Int, Int) [ NumberPosition ] deriving (Read, Show, Eq)

-- create long list which will be our table knowing x and y
createListOfLength :: Int -> Int -> [Int]
createListOfLength x y  = createListIterating (x*y) [] 

-- creaate list full of 0-os
createListIterating :: Int -> [Int] -> [Int]
createListIterating 0 _ = []
createListIterating n xs = 0 : createListIterating (n - 1) xs  
  
-- change position p in the grid s for value of x
tryWith :: Int -> [Int] -> Int -> [Int]
tryWith p s x = take p s ++ [x] ++ drop (p + 1) s 

tryWithForTable :: [Int] -> [Int] -> Int ->[Int]
tryWithForTable [] s x = []
tryWithForTable (p:[]) s x = tryWith p s x
tryWithForTable (p:px) s x = tryWithForTable px (tryWith p s x) x

-- converts an index i into an x and y co-ordinate
itop :: Int -> Int -> Int -> (Int, Int)
itop i x_size y_size = (calcX i, calcY i)
  where calcX i   = i - x_size * (i `div` x_size)
        calcY i   = i `div` y_size

-- takes an x and y co-ordinate and converts it into an index
ptoi :: (Int, Int) -> Int -> Int
ptoi (x, y) x_size = x + y * x_size  

numberpositiontoi :: NumberPosition -> Int -> Int
numberpositiontoi ((x,y),v) x_size = ptoi (x,y) x_size

numberpositionstoi :: [NumberPosition] -> Int -> [Int]
numberpositionstoi [] _ = []
numberpositionstoi (((x,y),v):xs ) x_size = (numberpositiontoi ((x,y),v) x_size) : numberpositionstoi xs x_size

-- retrieves the 2 x 2 square of values from the board (s) at the index (x,y)
squareAt :: NumberPosition -> Int -> Int -> [Int] -> [NumberPosition]
squareAt ((x,y),_) x_size y_size s | ( ( x==x_size ) && ( y==y_size ) ) = [ ((x-1,y-1), s !! ptoi (x-1, y-1) x_size )]
                                   | ( ( x==x_size ) && ( y==0 ) ) = [ ((x-1,y), s !! ptoi (x-1, y) x_size )]
                                   | ( ( x==0 ) && ( y==y_size ) ) = [ ((x,y-1), s !! ptoi (x, y-1) x_size )]
                                   | ( ( x/=0 ) && ( y==y_size ) ) = [ ((x-1,y-1),s !! ptoi (x-1, y-1) x_size ), ((x,y-1),s !! ptoi (x, y-1) x_size )]
                                   | ( ( x==x_size ) && ( y/=0 ) ) = [ ((x-1,y-1),s !! ptoi (x-1, y-1) x_size ), ((x-1,y), s !! ptoi (x-1, y) x_size )]
                                   | ( ( x/=0 ) && ( y/=0 ) ) = [((x-1,y-1), s !! ptoi (x-1, y-1) x_size ), ((x,y-1), s !! ptoi (x, y-1) x_size ), ((x-1,y), s !! ptoi (x-1, y) x_size ),((x,y), s !! ptoi (x, y) x_size )]
                                   | ( ( x==0 ) && ( y==0 ) ) = [ ((x,y), s !! ptoi (x, y) x_size )]
                                   | ( ( x/=0 ) && ( y==0 ) ) = [ ((x-1,y), s !! ptoi (x-1, y) x_size ), ((x,y),s !! ptoi (x, y) x_size )]               
                                   | ( ( x==0 ) && ( y/=0 ) ) = [ ((x,y-1), s !! ptoi (x, y-1) x_size), ((x,y),s !! ptoi (x, y) x_size )]

-- Find the next blank value starting from index p on board s
-- 80 is the index of the last element in s
nextBlank :: Int -> [Int] -> Int -> Int -> Int
nextBlank p s x_length y_length | p == x_length*y_length = x_length*y_length
                                | s !! (p + 1) == 0 = p + 1
                                | otherwise = nextBlank (p + 1) s x_length y_length 
								
-- function for deleting point from creek
deleteElementFromCreek :: Creek -> NumberPosition -> Creek
deleteElementFromCreek (Creek (x,y) list ) ((a,b),c) = Creek (x,y) (deleteElementsFromPointList list ((a,b),c))

-- delete NumberPosition from list													   
deleteElementsFromPointList :: [NumberPosition] -> NumberPosition -> [NumberPosition]
deleteElementsFromPointList [] _ = []
deleteElementsFromPointList ( ((a,b),c):xs ) ( (d,e),f ) | (a==d && b==e && c==f) = xs
                                                         | otherwise = ((a,b),c) : deleteElementsFromPointList xs ((d,e),f)
														 
-- initial numbers in table which will not be change throught the process
preprocess :: Creek -> [Int] -> [Int]
preprocess (Creek (_, _) []) xs = xs
preprocess (Creek (x_length, y_length) ( ((a,b),c) :xs ) ) board | (( (a==0 && b ==0) ||( a/=0 && b == 0) || (a/=0 && b==y_length) || (a==0 &&b/=0) || (a==x_length && b/=0)) && 
                                                                   (c == 0)) = preprocess (Creek (x_length, y_length) xs) 
																   (tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) 
																   x_length y_length board) x_length ) board 2) 
                                                                 | (( (a==0 && b == 0) ||( a/=0 && b == 0) || (a/=0 && b==y_length) || (a==0 &&b/=0) || (a==x_length && b/=0)) && c /= 0 &&
                                                                   (c == 2 || c == 3 || c == 4)) = preprocess (Creek(x_length, y_length)xs)
																   (tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) x_length y_length board) x_length ) board 1) 
                                                                 | ((a/=0 && b/=0) && (c == 0)) = preprocess (Creek (x_length, y_length) xs)
																    (tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) x_length y_length board) x_length ) board 2) 
																 | ((a/=0 && b/=0) && (c == 4)) = preprocess (Creek(x_length,y_length) xs) 
																 (tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) x_length y_length board) x_length ) board 1) 
                                                                 | ((a==0 && b ==0) && (c == 1)) = preprocess (Creek(x_length,y_length) xs) 
																	(tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) x_length y_length board) x_length ) board 1)
                                                                 | otherwise = preprocess (Creek(x_length,y_length) xs) board										 


main = do
    username <- promptCreek
    let creek = read username :: Creek
    persist creek
    --creek

promptCreek :: IO String
promptCreek = do
  putStrLn "Username to add to the password manager:"
  getLine

-- fake definitions to make things compile

persist :: Creek ->  IO ()
persist = print


