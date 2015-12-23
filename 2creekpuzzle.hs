module CreekPuzzle where

-- assumptions taken: 0 - on board is empty, 1 - is colored, 2 - must be empty
import Data.Array
import Data.List

-- user type to store position and it's value from Creek definition
type NumberPosition = ((Int,Int),Int)
type Node = (Int,Int)
type Edge = (Node,Node)
type Graph = ([Node],[Edge])

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

-- convert number position to index in table
numberpositiontoi :: NumberPosition -> Int -> Int
numberpositiontoi ((y,x),v) y_size = ptoi (y,x) y_size

-- list of numberpositions into indeces
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

-- is the left neighbour empty?
isLeftEmpty :: (Int,Int) -> [Int] -> Int-> Int -> Bool
isLeftEmpty (a,b) board y_size x_size | (isIndexInBounds y_size x_size (ptoi (a,b-1) y_size)) && (a >= 0 && a < x_size && b-1 >= 0 && b-1 < x_size) && (board !! ptoi (a,b-1) y_size == 0 || board !! ptoi (a,b-1) y_size == 2) = True
                                      | otherwise = False
-- is the right neighbour empty?
isRightEmpty :: (Int,Int) -> [Int] -> Int-> Int -> Bool
isRightEmpty (a,b) board y_size x_size | (isIndexInBounds y_size x_size (ptoi (a,b+1) y_size)) && (a >= 0 && a < x_size && b+1 >= 0 && b+1 < x_size) && (board !! ptoi (a,b+1) y_size == 0 || board !! ptoi (a,b+1) y_size == 2) = True
                                       | otherwise = False
-- is the top neighbour empty?									
isTopEmpty :: (Int,Int) -> [Int] -> Int-> Int -> Bool
isTopEmpty (a,b) board y_size x_size | (isIndexInBounds y_size x_size (ptoi (a-1,b) y_size)) && (a-1 >= 0 && a-1 < x_size && b >= 0 && b < x_size) && (board !! ptoi (a-1,b) y_size == 0 || board !! ptoi (a-1,b) y_size == 2) = True
                                     | otherwise = False
-- is the bottom neighbour empty?									
isBottomEmpty :: (Int,Int) -> [Int] -> Int-> Int -> Bool
isBottomEmpty (a,b) board y_size x_size | (isIndexInBounds y_size x_size (ptoi (a+1,b) y_size)) && (a+1 >= 0 && a+1 < x_size && b >= 0 && b < x_size) && (board !! ptoi (a+1,b) y_size == 0 || board !! ptoi (a+1,b) y_size == 2) = True
                                        | otherwise = False
-- i our index in bounds of the table?
isIndexInBounds :: Int -> Int -> Int -> Bool
isIndexInBounds y_size x_size i = (i >= 0) && (i < y_size*x_size)
 
depthfirst :: Graph -> Node -> [Node]
depthfirst (v,e) n
    | [x|x<-v,x==n] == [] = []
    | otherwise = dfrecursive (v,e) [n]
 
dfrecursive :: Graph -> [Node] -> [Node]
dfrecursive ([],_) _ = []
dfrecursive (_,_) [] = []
dfrecursive (v,e) (top:stack)
    | [x|x<-v,x==top] == [] = dfrecursive (newv, e) stack
    | otherwise = top : dfrecursive (newv, e) (adjacent ++ stack)
    where
        adjacent = [x | (x,y)<-e,y==top] ++ [x | (y,x)<-e,y==top]
        newv = [x|x<-v,x/=top]

isBoardCompact :: (Int,Int)->Graph->Bool
isBoardCompact p (v,e) = if (length (depthfirst (v,e) p) < length v) then
                            False
                         else
                            True

generateAllIndices :: [Int]-> Int -> Int-> [(Int,Int)]
generateAllIndices board y_size x_size = [(y,x) | y<-[0..y_size-1], x<-[0..x_size-1], board !! ptoi (y,x) y_size == 0 || board !! ptoi (y,x) y_size == 2 ]

generateAllEdges :: [(Int,Int)]->Int->Int->[Int]->[((Int,Int), (Int,Int))]
generateAllEdges [] _ _ _ = []
generateAllEdges ((y,x):xs) y_size x_size board | (isLeftEmpty (y,x) board y_size x_size && isRightEmpty (y,x) board y_size x_size && --top bottom left right
                                                   isTopEmpty (y,x) board y_size x_size && isBottomEmpty (y,x) board y_size x_size) = 
                                                  ((y,x),(y,x-1)):((y,x),(y,x+1)): ((y,x),(y-1,x)): ((y,x),(y+1,x)):generateAllEdges xs y_size x_size board
                                                | (isLeftEmpty (y,x) board y_size x_size  && isRightEmpty (y,x) board y_size x_size  && isTopEmpty (y,x) board y_size x_size )=
                                                  ((y,x),(y,x-1)):((y,x),(y,x+1)):((y,x),(y-1,x)):generateAllEdges xs y_size x_size board 
                                                | (isLeftEmpty (y,x) board y_size x_size  && isRightEmpty (y,x) board y_size x_size  && isBottomEmpty (y,x) board y_size x_size ) =
                                                  ((y,x),(y,x-1)):((y,x),(y,x+1)):((y,x),(y+1,x)):generateAllEdges xs y_size x_size board 
                                                | (isLeftEmpty (y,x) board y_size x_size  && isTopEmpty (y,x) board y_size x_size  && isBottomEmpty (y,x) board y_size x_size ) =
                                                  ((y,x),(y,x-1)):((y,x),(y-1,x)): ((y,x),(y+1,x)):generateAllEdges xs y_size x_size board 
                                                | (isRightEmpty (y,x) board y_size x_size  && isTopEmpty (y,x) board y_size x_size  && isBottomEmpty (y,x) board y_size x_size ) =
                                                  ((y,x),(y,x+1)):((y,x),(y-1,x)): ((y,x),(y+1,x)):generateAllEdges xs y_size x_size board 
                                                | (isLeftEmpty (y,x) board y_size x_size  && isTopEmpty (y,x) board y_size x_size) =
                                                  ((y,x),(y,x-1)):((y,x),(y-1,x)):generateAllEdges xs y_size x_size board 
                                                | (isLeftEmpty (y,x) board y_size x_size  && isBottomEmpty (y,x) board y_size x_size) =
                                                  ((y,x),(y,x-1)):((y,x),(y+1,x)):generateAllEdges xs y_size x_size board 
                                                | (isLeftEmpty (y,x) board y_size x_size  && isRightEmpty (y,x) board y_size x_size) =
                                                  ((y,x),(y,x-1)): ((y,x),(y,x+1)):generateAllEdges xs y_size x_size board 
                                                | (isRightEmpty (y,x) board y_size x_size  && isTopEmpty (y,x) board y_size x_size) =
                                                  ((y,x),(y,x+1)):((y,x),(y-1,x)):generateAllEdges xs y_size x_size board 
                                                | (isRightEmpty (y,x) board y_size x_size  && isBottomEmpty (y,x) board y_size x_size) =
                                                  ((y,x),(y,x+1)):((y,x),(y+1,x)):generateAllEdges xs y_size x_size board
                                                | (isTopEmpty (y,x) board y_size x_size  && isBottomEmpty (y,x) board y_size x_size) = 
                                                  ((y,x),(y-1,x)): ((y,x),(y+1,x)):generateAllEdges xs y_size x_size board 
                                                | (isLeftEmpty (y,x) board y_size x_size ) = 
                                                  ((y,x),(y,x-1)): generateAllEdges xs y_size x_size board 
                                                | (isRightEmpty (y,x) board y_size x_size ) = 
                                                  ((y,x),(y,x+1)): generateAllEdges xs y_size x_size board 
                                                | (isTopEmpty (y,x) board y_size x_size ) = 
                                                  ((y,x),(y-1,x)): generateAllEdges xs y_size x_size board
                                                | (isBottomEmpty (y,x) board y_size x_size ) = 
                                                  ((y,x),(y+1,x)): generateAllEdges xs y_size x_size board 
                                                | otherwise = generateAllEdges xs y_size x_size board

processAllEdges :: [((Int,Int), (Int,Int))]->[((Int,Int), (Int,Int))]->[((Int,Int), (Int,Int))]
processAllEdges [] xs = xs
processAllEdges (((a,b),(c,d)):xs) edges =  if (elem ((c,d),(a,b)) edges ) then
                                                processAllEdges xs (removeItem ((a,b),(c,d)) edges)
                                            else
                                                processAllEdges xs edges

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = ys
                    | otherwise = y : removeItem x ys

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

howManyColoured:: [NumberPosition] -> Int
howManyColoured [] = 0
howManyColoured (((a,b),c):xs) = if (c == 1) then
                                    1 + howManyColoured xs
                                 else
                                    howManyColoured xs

getEmptiesin4:: [NumberPosition] -> [NumberPosition]
getEmptiesin4 [] = []
getEmptiesin4(((a,b),c):xs) | (c==0) = ((a,b),c):getEmptiesin4 xs 
                            | otherwise = getEmptiesin4 xs

solve :: [NumberPosition]-> [Int] -> [[Int]] ->Int->Int->[NumberPosition]->[Int]
solve [] s _ _ _ _ = s
solve (p:ps) s [[]] y_size x_size np  = solve ps s (returnPossibleSolutions (head ps) s y_size x_size) y_size x_size np
solve [p] s (x:xs) y_size x_size np | (value == True ) = (tryWithForTable x s 1) 
                                       | otherwise = solve [p] s xs y_size y_size np
                                        where vertices = generateAllIndices (tryWithForTable x s 1) y_size x_size;
                                              vertex_head = head vertices;
                                              edges = generateAllEdges vertices y_size x_size s;
                                              edges1 = processAllEdges edges edges;
                                              graph = (vertices, edges1);
										      check = checkConditions  np (tryWithForTable x s 1) y_size x_size;;
											  compact = isBoardCompact vertex_head graph;
											  value = compact && check
											  
solve (p:ps) s (x:xs) y_size x_size np | (value == True ) = solve ps (tryWithForTable x s 1) (returnPossibleSolutions (head ps) s y_size x_size) y_size x_size (p:np)
                                       | otherwise = solve (p:ps) s xs y_size x_size np  
                                        where vertices = generateAllIndices (tryWithForTable x s 1) y_size x_size;
                                              vertex_head = head vertices;
                                              edges = generateAllEdges vertices y_size x_size s;
                                              edges1 = processAllEdges edges edges;
                                              graph = (vertices, edges1);
										      check = checkConditions  np (tryWithForTable x s 1) y_size x_size;
											  compact = isBoardCompact vertex_head graph;
											  value = compact && check


returnPossibleSolutions:: NumberPosition ->[Int]->Int->Int->[[Int]]
returnPossibleSolutions ((a,b),c) board y_size x_size = combinations howManyOutOf4 (numberpositionstoi (getEmptiesin4 (squareAt  ((a,b),c) y_size x_size board  )) y_size)
                                                where howMany = howManyColoured (squareAt ((a,b),c) y_size x_size board);
                                                      howManyOutOf4 = c - howMany;
x = [0,1,1,2,0,1,1,2,0,0,0,0,0,0,0,0] 
solveIt = solve [ ((1, 0), 1), ((1, 2), 2), ((2, 3), 1)] x (returnPossibleSolutions ((1,0),1) x  4 4) 4 4 [] 

qwey = [0,1,1,2,0,1,1,2,0,1,0,0,0,0,0,0]
solveIt2 = solve [ ((3,3),1)] qwey (returnPossibleSolutions ((3,3),1) qwey  4 4) 4 4 [   ((1, 0), 1), ((1, 2), 2), ((2, 3), 1), ((3,3),1)] 

solveIt3 = solve [ ((1, 0), 1), ((1, 2), 2), ((2, 3), 1), ((3,3),1)] x (returnPossibleSolutions ((1,0),1) x  4 4) 4 4 [ ] 

checkConditions :: [NumberPosition]->[Int] ->Int->Int->Bool
checkConditions  [] _ _ _ = True
checkConditions  [((a,b),c)] board y_size x_size = if (howManyColoured (squareAt ((a,b),c) y_size x_size board) == c) then
                                                                True
                                                            else
                                                                False
checkConditions  (((a,b),c):ps) board y_size x_size | (howManyColoured (squareAt ((a,b),c) y_size x_size board) == c) = checkConditions  ps board y_size x_size
                                                             | otherwise = False

getListFromCreek:: Creek -> [NumberPosition]
getListFromCreek (Creek (_,_) list) = list

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

-- delete all used elements
deletePreprocessFromCreek :: Creek -> [NumberPosition] -> Creek
deletePreprocessFromCreek (Creek (y,x) list) [] = (Creek (y,x) list)
deletePreprocessFromCreek (Creek (y,x) list) (n:ns) = deletePreprocessFromCreek (deleteElementFromCreek  (Creek(y,x) list) n)   ns

-- create list of all preprocessed elements already used in the solution
createUsedPreprocessList :: Creek -> [NumberPosition]
createUsedPreprocessList (Creek (y_length,x_length) []) = []
createUsedPreprocessList (Creek (y_length, x_length) ( ((a,b),c) :xs ) ) | (( (a==0 && b ==0) ||( a/=0 && b == 0) || (a/=0 && b==x_length) || (a==0 &&b/=0) || (a==y_length && b/=0)) && 
                                                                           (c == 0 || c == 2 || c == 3 || c == 4)) ||
                                                                           ( ((a/=0 && b/=0) && (c == 0)) || ((a/=0 && b/=0) && (c == 4)) || ((a==0 && b ==0) && (c == 1)) )
                                                                            = ((a,b),c) : createUsedPreprocessList (Creek(y_length,x_length) xs)
                                                                         | otherwise = createUsedPreprocessList (Creek (y_length, x_length) xs)
-- Creek after deleting obvious assumptions from it
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
printResult y_length board = pPrint (changeIntsIntoChars( change2dInto1D ( (transpose (change1dinto2d y_length board))) )) y_length 

changeIntsIntoChars:: [Int] -> [Int]
changeIntsIntoChars [] = []
changeIntsIntoChars (x:xs) | (x == 1) = 1:changeIntsIntoChars xs
                           | otherwise = 0:changeIntsIntoChars xs

totalSolution :: Creek -> String
totalSolution (Creek (y, x) list) = printResult y (solve  creekList postTable firstPossibleSolution  y x [])
                                  where creekList = getListFromCreek ( postprocessedCreek(Creek(y,x) list) );
								        postTable = preprocess (Creek(y,x) list) (createListOfLength y x);
										firstPossibleSolution = returnPossibleSolutions ( head creekList) postTable y x;
								    
main = do 
        file_name <- promptCreek;
        creek_text <- readFile file_name;
      --let people :: [Creek]
        let creek = read creek_text :: Creek
        persist creek;
		putStrLn (totalSolution creek)

promptCreek :: IO String
promptCreek = do
  putStrLn "Podaj nazwę pliku zawierającego łamigłówkę:"
  getLine

-- fake definitions to make things compile

persist :: Creek ->  IO ()
persist = print


