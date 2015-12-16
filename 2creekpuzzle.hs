module CreekPuzzle where

import Data.Array
import Data.List

-- where we will be solving our puzzle
type CreekBoard = [[Int]]
-- position of a cell in CreekBoard
type Position = (Int, Int)

type NumberPosition = ((Int,Int),Int)

-- user data to read CreekPuzzle description
data Creek = Empty | Creek (Int, Int) [ NumberPosition ] deriving (Read, Show)

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
  
-- converts an index i into an x and y co-ordinate
itop :: Int -> Int -> Int -> (Int, Int)
itop i x_size y_size = (calcX i, calcY i)
  where calcX i   = i - x_size * (i `div` x_size)
        calcY i   = i `div` y_size

-- Takes an x and y co-ordinate and converts it into an index
ptoi :: (Int, Int) -> Int -> Int
ptoi (x, y) x_size = x + y * x_size  

-- Retrieves the 3 x 3 square of values from the board (s) at the index (p)
squareAt :: NumberPosition -> Int -> Int -> [Int] -> [NumberPosition]
squareAt ((x,y),v) x_size y_size s | ( ( x==x_size ) && ( y==y_size ) ) = [ ((x-1,y-1), s !! ptoi (x-1, y-1) x_size )]
                                   | ( ( x==x_size ) && ( y==0 ) ) = [ ((x-1,y), s !! ptoi (x-1, y) x_size )]
                                   | ( ( x==0 ) && ( y==y_size ) ) = [ ((x,y-1), s !! ptoi (x, y-1) x_size )]
                                   | ( ( x/=0 ) && ( y==y_size ) ) = [ ((x-1,y-1),s !! ptoi (x-1, y-1) x_size ), ((x,y-1),s !! ptoi (x, y-1) x_size )]
                                   | ( ( x==x_size ) && ( y/=0 ) ) = [ ((x-1,y-1),s !! ptoi (x-1, y-1) x_size ), ((x-1,y), s !! ptoi (x-1, y) x_size )]
                                   | ( ( x/=0 ) && ( y/=0 ) ) = [((x-1,y-1), s !! ptoi (x-1, y-1) x_size ), ((x,y-1), s !! ptoi (x, y-1) x_size ), ((x-1,y), s !! ptoi (x-1, y) x_size ),((x,y), s !! ptoi (x, y) x_size )]
                                   | ( ( x==0 ) && ( y==0 ) ) = [ ((x,y), s !! ptoi (x, y) x_size )]
                                   | ( ( x/=0 ) && ( y==0 ) ) = [ ((x-1,y), s !! ptoi (x-1, y) x_size ), ((x,y),s !! ptoi (x, y) x_size )]               
                                   | ( ( x==0 ) && ( y/=0 ) ) = [ ((x,y-1), s !! ptoi (x, y-1) x_size), ((x,y),s !! ptoi (x, y) x_size )]
                                  
  
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


