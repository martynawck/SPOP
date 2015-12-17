module CreekPuzzle where

-- assumptions taken: 0 - on board is empty, 1 - is colored, 2 - must be empty , 3 - must remain colored
import Data.Array
import Data.List

-- where we will be solving our puzzle
--type CreekBoard = [[Int]]
-- position of a cell in CreekBoard
--type Position = (Int, Int)

-- user type to store position and it's value from Creek definition
type NumberPosition = ((Int,Int),Int)

-- user data to read CreekPuzzle description
data Creek = Empty | Creek (Int, Int) [ NumberPosition ] deriving (Read, Show, Eq)

-- create long 1d list which will be our table (size is x*y)
createListOfLength :: Int -> Int -> [Int]
createListOfLength y x  = createListIterating (y*x) [] 

-- creaate list full of 0-os of a given size n
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
itop i y_size x_size = (calcY i, calcX i)
  where calcY i   = i - y_size * (i `div` y_size)
        calcX i   = i `div` x_size

-- takes an x and y co-ordinate and converts it into an index
ptoi :: (Int, Int) -> Int -> Int
ptoi (y, x) y_size = y + x * y_size  

numberpositiontoi :: NumberPosition -> Int -> Int
numberpositiontoi ((y,x),v) y_size = ptoi (y,x) y_size

numberpositionstoi :: [NumberPosition] -> Int -> [Int]
numberpositionstoi [] _ = []
numberpositionstoi (((y,x),v):xs ) y_size = (numberpositiontoi ((y,x),v) y_size) : numberpositionstoi xs y_size

-- retrieves the 2 x 2 square of values from the board (s) at the index (x,y)
squareAt :: NumberPosition -> Int -> Int -> [Int] -> [NumberPosition]
squareAt ((y,x),_) y_size x_size s | ( ( x==x_size ) && ( y==y_size ) ) = [ ((y-1,x-1), s !! ptoi (y-1, x-1) y_size )]
                                   | ( ( x==x_size ) && ( y==0 ) ) = [ ((y,x-1), s !! ptoi (y,x-1) y_size )]
                                   | ( ( x==0 ) && ( y==y_size ) ) = [ ((y-1,x), s !! ptoi (y-1,x) y_size )]
                                   | ( ( x/=0 ) && ( y==y_size ) ) = [ ((y-1,x-1),s !! ptoi (y-1, x-1) y_size ), ((y-1,x),s !! ptoi (y-1,x) y_size )]
                                   | ( ( x==x_size ) && ( y/=0 ) ) = [ ((y-1,x-1),s !! ptoi (y-1, x-1) y_size ), ((y,x-1), s !! ptoi (y,x-1) y_size )]
                                   | ( ( x/=0 ) && ( y/=0 ) ) = [((y-1,x-1), s !! ptoi (y-1,x-1) y_size ), ((y-1,x), s !! ptoi (y-1,x) y_size ), ((y,x-1), s !! ptoi (y,x-1) y_size ),((y,x), s !! ptoi (y,x) y_size )]
                                   | ( ( x==0 ) && ( y==0 ) ) = [ ((y,x), s !! ptoi (y,x) y_size )]
                                   | ( ( x/=0 ) && ( y==0 ) ) = [ ((y,x-1), s !! ptoi (y,x-1) y_size ), ((y,x),s !! ptoi (y,x) y_size )]               
                                   | ( ( x==0 ) && ( y/=0 ) ) = [ ((y-1,x), s !! ptoi (y-1,x) y_size), ((y,x),s !! ptoi (y,x) y_size )]

-- Find the next blank value starting from index p on board s
-- 80 is the index of the last element in s
nextBlank :: Int -> [Int] -> Int -> Int -> Int
nextBlank p s y_length x_length | p == y_length*x_length = y_length*x_length
                                | s !! (p + 1) == 0 = p + 1
                                | otherwise = nextBlank (p + 1) s y_length x_length 
								
-- function for deleting point from creek
deleteElementFromCreek :: Creek -> NumberPosition -> Creek
deleteElementFromCreek (Creek (y,x) list ) ((a,b),c) = Creek (y,x) (deleteElementsFromPointList list ((a,b),c))

-- delete NumberPosition from list													   
deleteElementsFromPointList :: [NumberPosition] -> NumberPosition -> [NumberPosition]
deleteElementsFromPointList [] _ = []
deleteElementsFromPointList ( ((a,b),c):xs ) ( (d,e),f ) | (a==d && b==e && c==f) = xs
                                                         | otherwise = ((a,b),c) : deleteElementsFromPointList xs ((d,e),f)
														 
-- initial numbers in table which will not be change throughout the process
preprocess :: Creek -> [Int] -> [Int]
preprocess (Creek (_, _) []) xs = xs
preprocess (Creek (y_length, x_length) ( ((a,b),c) :xs ) ) board | (( (a==0 && b ==0) ||( a/=0 && b == 0) || (a/=0 && b==x_length) || (a==0 &&b/=0) || (a==y_length && b/=0)) && 
                                                                   (c == 0)) = preprocess (Creek (y_length, x_length) xs) 
                                                                   (tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) 
																    y_length x_length board) y_length ) board 2) 
                                                                 | (( (a==0 && b == 0) ||( a/=0 && b == 0) || (a/=0 && b==x_length) || (a==0 &&b/=0) || (a==y_length && b/=0)) && c /= 0 &&
                                                                   (c == 2 || c == 3 || c == 4)) = preprocess (Creek(y_length, x_length)xs)
																   (tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) y_length x_length board) y_length ) board 1) 
                                                                 | ((a/=0 && b/=0) && (c == 0)) = preprocess (Creek (y_length, x_length) xs)
																    (tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) y_length x_length board) y_length ) board 2) 
																 | ((a/=0 && b/=0) && (c == 4)) = preprocess (Creek(y_length,x_length) xs) 
																 (tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) y_length x_length board) y_length ) board 1) 
                                                                 | ((a==0 && b ==0) && (c == 1)) = preprocess (Creek(y_length,x_length) xs) 
																	(tryWithForTable ( numberpositionstoi ( squareAt((a,b),c) y_length x_length board) y_length ) board 1)
                                                                 | otherwise = preprocess (Creek(y_length,x_length) xs) board										 


deletePreprocessFromCreek :: Creek -> [NumberPosition] -> Creek
deletePreprocessFromCreek (Creek (y,x) list) [] = (Creek (y,x) list)
deletePreprocessFromCreek (Creek (y,x) list) (n:ns) = deletePreprocessFromCreek (deleteElementFromCreek  (Creek(y,x) list) n)   ns

createUsedPreprocessList :: Creek -> [NumberPosition]
createUsedPreprocessList (Creek (y_length,x_length) []) = []
createUsedPreprocessList (Creek (y_length, x_length) ( ((a,b),c) :xs ) ) | (( (a==0 && b ==0) ||( a/=0 && b == 0) || (a/=0 && b==x_length) || (a==0 &&b/=0) || (a==y_length && b/=0)) && 
                                                                           (c == 0 || c == 2 || c == 3 || c == 4)) ||
                                                                           ( ((a/=0 && b/=0) && (c == 0)) || ((a/=0 && b/=0) && (c == 4)) || ((a==0 && b ==0) && (c == 1)) )
                                                                            = ((a,b),c) : createUsedPreprocessList (Creek(y_length,x_length) xs)
                                                                         | otherwise = createUsedPreprocessList (Creek (y_length, x_length) xs)
postprocessedCreek :: Creek -> Creek
postprocessedCreek (Creek(y_length, x_length) list) = deletePreprocessFromCreek (Creek(y_length, x_length) list) (createUsedPreprocessList (Creek(y_length,x_length) list) )					
	
-- intersperse the element c through-out the string xs
joinWith :: a -> [a] -> [a]
joinWith _ (x:[])  = [x]
joinWith c (x:xs)  = x : c : joinWith c xs

-- Pretty-print the board as a spaced out 9 x 9 square
pPrint [] _ = []
pPrint s y_length = spaceOut s ++ pPrint (drop y_length s) y_length
  where showS s    = concatMap show s
        space      = ' '
        newline    = "\n"
        spaceOut s = joinWith space (take y_length (showS s) ++ newline) 
	
--change array of arrays into simple list	
change2dInto1D ::[[Int]] -> [Int]
change2dInto1D  [] = []
change2dInto1D ((x:[]):ys) = x:change2dInto1D(ys)
change2dInto1D ((x:xs):ys) = x:change2dInto1D((xs):ys)

--change simple list into array of arrays
change1dinto2d :: Int -> [Int] -> [[Int]]
change1dinto2d _ [] = []
change1dinto2d n l
         | n > 0 = (take n l) : (change1dinto2d n (drop n l))
         | otherwise = error "Negative n"
-- print final result like a table -> maybe instead 1 and 2 print X	
printResult:: Int-> [Int] -> String
printResult y_length board = pPrint ( change2dInto1D ( (transpose (change1dinto2d y_length board))) ) y_length 


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


