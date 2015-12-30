module CreekPuzzle where

-- założenie programu: 0 - puste pole, 1 - pole zamalowane, 2 - pole, które musi zostać puste
import Data.Array
import Data.List
import Debug.Trace

-- typy danych, zdefiniowane przez użytkownika
-- opis przecięcia 4 elementów planszy
type IntersectionDescription = ((Int,Int),Int)
-- węzeł w grafie
type Node = (Int,Int)
-- krawędź w grafie
type Edge = (Node,Node)
-- opis grafu
type Graph = ([Node],[Edge])

-- typ danych, służący do przechowania opisu planszy Creek
data Creek = Empty | Creek (Int, Int) [ IntersectionDescription ] deriving (Read, Show, Eq)

-- stworzenie listy o wymiarach y,x (o długości y*x)
createListOfLength :: Int -> Int -> [Int]
createListOfLength y x  = createListIterating (y*x) [] 

--funkcja pomocnicza do stworzenia listy wypełnionej zerami
createListIterating :: Int -> [Int] -> [Int]
createListIterating 0 _ = []
createListIterating n xs = 0 : createListIterating (n - 1) xs  
  
-- zmiana elementu na pozycji p w tabeli s na element x
tryWith :: Int -> [Int] -> Int -> [Int]
tryWith p s x = take p s ++ [x] ++ drop (p + 1) s 

-- zmiana elementów podanych w liście na pozycjach w tabeli s na element x (wszystkie elementy z listy wymieniane są na element x)
tryWithForTable :: [Int] -> [Int] -> Int ->[Int]
tryWithForTable [] s x = []
tryWithForTable (p:[]) s x = tryWith p s x
tryWithForTable (p:px) s x = tryWithForTable px (tryWith p s x) x

-- konwersja indeksu i na współrzędne (y, x)
itop :: Int -> Int -> Int -> (Int, Int)
itop i y_size x_size = (calcY i, calcX i)
  where calcY i   = i - y_size * (i `div` y_size)
        calcX i   = i `div` x_size

-- konwersja współrzędnej w postaci (y, x) na indeks i tabelis
ptoi :: (Int, Int) -> Int -> Int
ptoi (y, x) y_size = y + x * y_size  

-- konwersja typu IntersectionDescription na indeks w tabeli
intersectionDescriptiontoi :: IntersectionDescription -> Int -> Int
intersectionDescriptiontoi ((y,x),v) y_size = ptoi (y,x) y_size

-- konwersja listy typów IntersectionDescriptions na listę indeksów tabeli
intersectionDescriptionstoi :: [IntersectionDescription] -> Int -> [Int]
intersectionDescriptionstoi [] _ = []
intersectionDescriptionstoi (((y,x),v):xs ) y_size = (intersectionDescriptiontoi ((y,x),v) y_size) : intersectionDescriptionstoi xs y_size

-- funkcja znajdująca kwadrat 2x2 dla danego IntersectionDescription w tabeli - jest to kwadrat opisywany przez dany IntersectionDescription
squareAt :: IntersectionDescription -> Int -> Int -> [Int] -> [IntersectionDescription]
squareAt ((y,x),_) y_size x_size s | ( ( x==x_size ) && ( y==y_size ) ) = [ ((y-1,x-1), s !! ptoi (y-1, x-1) y_size )]
                                   | ( ( x==x_size ) && ( y==0 ) ) = [ ((y,x-1), s !! ptoi (y,x-1) y_size )]
                                   | ( ( x==0 ) && ( y==y_size ) ) = [ ((y-1,x), s !! ptoi (y-1,x) y_size )]
                                   | ( ( x/=0 ) && ( y==y_size ) ) = [ ((y-1,x-1),s !! ptoi (y-1, x-1) y_size ), ((y-1,x),s !! ptoi (y-1,x) y_size )]
                                   | ( ( x==x_size ) && ( y/=0 ) ) = [ ((y-1,x-1),s !! ptoi (y-1, x-1) y_size ), ((y,x-1), s !! ptoi (y,x-1) y_size )]
                                   | ( ( x/=0 ) && ( y/=0 ) ) = [((y-1,x-1), s !! ptoi (y-1,x-1) y_size ), ((y-1,x), s !! ptoi (y-1,x) y_size ), ((y,x-1), s !! ptoi (y,x-1) y_size ),((y,x), s !! ptoi (y,x) y_size )]
                                   | ( ( x==0 ) && ( y==0 ) ) = [ ((y,x), s !! ptoi (y,x) y_size )]
                                   | ( ( x/=0 ) && ( y==0 ) ) = [ ((y,x-1), s !! ptoi (y,x-1) y_size ), ((y,x),s !! ptoi (y,x) y_size )]               
                                   | ( ( x==0 ) && ( y/=0 ) ) = [ ((y-1,x), s !! ptoi (y-1,x) y_size), ((y,x),s !! ptoi (y,x) y_size )]

-- czy lewy sąsiad jest pusty?
isLeftEmpty :: (Int,Int) -> [Int] -> Int-> Int -> Bool
isLeftEmpty (a,b) board y_size x_size | (isIndexInBounds y_size x_size (ptoi (a,b-1) y_size)) && (a >= 0 && a < x_size && b-1 >= 0 && b-1 < x_size) && (board !! ptoi (a,b-1) y_size == 0 || board !! ptoi (a,b-1) y_size == 2) = True
                                      | otherwise = False
-- czy prawy sąsiad jest pusty?
isRightEmpty :: (Int,Int) -> [Int] -> Int-> Int -> Bool
isRightEmpty (a,b) board y_size x_size | (isIndexInBounds y_size x_size (ptoi (a,b+1) y_size)) && (a >= 0 && a < x_size && b+1 >= 0 && b+1 < x_size) && (board !! ptoi (a,b+1) y_size == 0 || board !! ptoi (a,b+1) y_size == 2) = True
                                       | otherwise = False
-- czy górny sąsiad jest pusty?								
isTopEmpty :: (Int,Int) -> [Int] -> Int-> Int -> Bool
isTopEmpty (a,b) board y_size x_size | (isIndexInBounds y_size x_size (ptoi (a-1,b) y_size)) && (a-1 >= 0 && a-1 < x_size && b >= 0 && b < x_size) && (board !! ptoi (a-1,b) y_size == 0 || board !! ptoi (a-1,b) y_size == 2) = True
                                     | otherwise = False
-- czy dolny sąsiad jest pusty?									
isBottomEmpty :: (Int,Int) -> [Int] -> Int-> Int -> Bool
isBottomEmpty (a,b) board y_size x_size | (isIndexInBounds y_size x_size (ptoi (a+1,b) y_size)) && (a+1 >= 0 && a+1 < x_size && b >= 0 && b < x_size) && (board !! ptoi (a+1,b) y_size == 0 || board !! ptoi (a+1,b) y_size == 2) = True
                                        | otherwise = False
-- czy indeks i, który rozpatrujemy mieści się w granicach tabeli?
isIndexInBounds :: Int -> Int -> Int -> Bool
isIndexInBounds y_size x_size i = (i >= 0) && (i < y_size*x_size)
 
-- algorytm dfs do przeszukwiania grafu
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

-- czy tablica jest spójna? (czy dla jednego elementu pustego lista elementów, do których możemy dotrzeć jest równa ilości tych elementów?)
isBoardCompact :: (Int,Int)->Graph->Bool
isBoardCompact p (v,e) = if (length (depthfirst (v,e) p) < length v) then
                            False
                         else
                            True

isBoardCompact2 :: (Int,Int)->Graph->[Int]->Creek->Bool
isBoardCompact2 p (v,e) board creek = if (isSubset (generateNodesDescribedByCreek board creek) (depthfirst (v,e) p)) then
                            True
                         else
                            False

addIfNonExistentMultiple :: [Node]->[Node]->[Node]
addIfNonExistentMultiple [] t = t
addIfNonExistentMultiple (x:xs) t | (not (elem x t)) =  addIfNonExistentMultiple xs (x:t)
                                  | otherwise = addIfNonExistentMultiple xs t


--pierwszy mniejszy niz drugi
isSubset:: Eq a => [a]->[a]->Bool
isSubset [] _ = True
isSubset (x:xs) y | (elem x y) = isSubset xs y
                  | otherwise = False

getPossibleCreekDescriptors:: Node -> [Node]
getPossibleCreekDescriptors (y,x) = [(y,x), (y,x+1), (y+1,x), (y+1,x+1)]

anyElem :: [Node] -> [Node] -> Bool
anyElem [] _ = False
anyElem (x:xs) list | (elem x list) = True
                    | otherwise = anyElem xs list						   
					
intersectionDescriptionToNodes :: [IntersectionDescription] ->[Node]
intersectionDescriptionToNodes [((a,b),_)] = [(a,b)]
intersectionDescriptionToNodes (((a,b),_):list) = (a,b):intersectionDescriptionToNodes list

blah :: [Node] -> [IntersectionDescription] ->[Node]->[Node]
blah [] is acc = acc
blah (p:ps) is acc = if (anyElem (getPossibleCreekDescriptors(p)) (intersectionDescriptionToNodes is)  )  then
                       blah ps is (addIfNonExistentMultiple [p] acc)
					 else
					  blah ps is acc
					  
blah1 :: [Node] -> [IntersectionDescription] ->[Node]->[Node]
blah1 [] is acc = acc
blah1 (p:ps) is acc = if ( not(anyElem (getPossibleCreekDescriptors(p)) (intersectionDescriptionToNodes is) ) )  then
                       blah1 ps is (addIfNonExistentMultiple [p] acc)
					 else
					  blah1 ps is acc
					  
generateNodesDescribedByCreek :: [Int]->Creek->[Node]
generateNodesDescribedByCreek board (Creek (y,x) list) = blah ( generateAllNodes board y x) list []                   

generateNodesNOTDescribedByCreek :: [Int]->Creek->[Node]
generateNodesNOTDescribedByCreek board (Creek (y,x) list) = blah1 ( generateAllNodes board y x) list []                   


-- funkcja generująca wszystkie węzły w grafie (czyli takie, które w naszej tablicy opisywane są jako puste)
generateAllNodes :: [Int]-> Int -> Int-> [Node]
generateAllNodes board y_size x_size = [(y,x) | y<-[0..y_size-1], x<-[0..x_size-1], board !! ptoi (y,x) y_size == 0 || board !! ptoi (y,x) y_size == 2 ]

-- funkcja generująca wszystkie krawędzie w grafie (krawędzie łączace puste pola z pustymi)
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

-- funkcja usuwająca powtarzające się krawędzie
processAllEdges :: [((Int,Int), (Int,Int))]->[((Int,Int), (Int,Int))]->[((Int,Int), (Int,Int))]
processAllEdges [] xs = xs
processAllEdges (((a,b),(c,d)):xs) edges =  if (elem ((c,d),(a,b)) edges ) then
                                                processAllEdges xs (removeItem ((a,b),(c,d)) edges)
                                            else
                                                processAllEdges xs edges
-- funkcja pomocnicza do usunięcia elementu z listy
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = ys
                    | otherwise = y : removeItem x ys

-- funkcja generująca wszystkie kombinacje rozwiązań
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

howManyColoured:: [IntersectionDescription] -> Int
howManyColoured [] = 0
howManyColoured (((a,b),c):xs) = if (c == 1) then
                                    1 + howManyColoured xs
                                 else
                                    howManyColoured xs
-- funkcja znajdująca wszystkie oznaczenia pustych miejsc w tabeli
getEmptiesin4:: [IntersectionDescription] -> [IntersectionDescription]
getEmptiesin4 [] = []
getEmptiesin4(((a,b),c):xs) | (c==0) = ((a,b),c):getEmptiesin4 xs 
                            | otherwise = getEmptiesin4 xs

							
solve2 :: [Node]->[Int]->Creek->[Int]
solve2 [] s _ = s
solve2 (p:ps) s (Creek (y,x) list) = if (compact == True) then
                                        solve2 ps (tryWith (ptoi p y) s 1) (Creek (y,x) list)
					                 else 
						               	solve2 ps s (Creek (y,x) list)
						             where vertices = generateAllNodes (tryWith (ptoi p y) s 1) y x;
									 edges = generateAllEdges vertices y x (tryWith (ptoi p y) s 1);
									 edges1 = processAllEdges edges edges;
									 graph = (vertices, edges1);
									 nodesdescr = generateNodesDescribedByCreek (tryWith (ptoi p y) s 1) (Creek (y,x) list);
									 compact = isBoardCompact2 (head nodesdescr) graph (tryWith (ptoi p y) s 1) (Creek (y,x) list)
 	
-- funkcja rozwiązująca łamigłówkę rekurencyjnie
solve :: [IntersectionDescription]-> [Int] -> [[Int]] ->Int->Int->[IntersectionDescription]->Creek->[Int]
--solve [] s _ _ _ _ = s
-- nie ma juz nic do rozwiazywania
solve [p] s [[]] _ _ _ _ = (trace ("1 ")) s
solve (p:ps) s [[]] y_size x_size np creek = (trace ("2 ")) solve ps s (returnPossibleSolutions (head ps) s y_size x_size) y_size x_size (p:np) creek
-- rozwiazania sie wyczerpaly
solve [] s [] _ _ _ _ = (trace ("3 "))s
solve [p] s [] _ _ _ _ = (trace ("4 "))[]
solve (p:ps) s [] _ _ _ _ = (trace ("5 ")) []


solve [p] s (x:xs) y_size x_size np creek | (trace ("6 ")) (value == True ) = (tryWithForTable x s 1) 
                                          | (trace ("7 "))otherwise = solve [p] s xs y_size y_size np creek
                                        where vertices = generateAllNodes (tryWithForTable x s 1) y_size x_size;
                                              --vertex_head = head vertices;
											  gndbc = generateNodesDescribedByCreek (tryWithForTable x s 1) creek;
											  vertex_head = head gndbc;
                                              edges = generateAllEdges vertices y_size x_size s;
                                              edges1 = processAllEdges edges edges;
                                              graph = (vertices, edges1);
                                              check = checkConditions  np (tryWithForTable x s 1) y_size x_size;
                                              compact = isBoardCompact2 vertex_head graph (tryWithForTable x s 1) creek;
                                              value = {-(trace ("s " ++ show s ++ " p " ++ show p ++ "x " ++ show x ++ " xs " ++ show xs ++ " check " ++ show check ++ " compact " ++ show compact) ) -}compact && check
solve (p:ps) s (x:xs) y_size x_size np creek |(trace ("8 " {-++ "p" ++ show p++" x" ++ show x ++ "xs" ++ show xs-}))(value == False || solvedNext == [] ) =  solve (p:ps) s xs y_size x_size np creek
											-- |(trace ("9 "))(value == True ) = solveNext p (tryWithForTable x s 1)
                                             | (trace ("10 ")) otherwise = solvedNext
                                        where vertices = generateAllNodes (tryWithForTable x s 1) y_size x_size;
                                           --   vertex_head = head vertices;
                                              edges = generateAllEdges vertices y_size x_size s;
                                              edges1 = processAllEdges edges edges;
                                              graph = (vertices, edges1);
											  gndbc = generateNodesDescribedByCreek (tryWithForTable x s 1) creek;
											  vertex_head = head gndbc;
                                              check = checkConditions  np (tryWithForTable x s 1) y_size x_size;
                                              compact = isBoardCompact2 vertex_head graph (tryWithForTable x s 1) creek;
                                              value = {-(trace ("gndbc" ++ show gndbc ++ "compact   " ++show compact))-}compact && check;
											  solveNext p s = {-(trace ("snext " ++ show s))-} solve ps s (returnPossibleSolutions (head ps) s y_size x_size) y_size x_size (p:np) creek;
											  solvedNext = {-(trace ("s " ++ show s ++ " p " ++ show p ++ " xs " ++ show xs ++" check " ++ show check ++ " compact " ++ show compact ++ " ps " ++ show ps) ) -}solveNext p (tryWithForTable x s 1)


{-solve [p] s (x:xs) y_size x_size np creek    | (solvedNext == [] && value == True) =  trace ("head: ") solvedNext
                                             | (solvedNext == [] || value == False) =  trace ("head: ") solve (p:ps) s xs y_size x_size np creek
--											 |(value == True ) = solve ps (tryWithForTable x s 1) (returnPossibleSolutions (head ps) s y_size x_size) y_size x_size (p:np) creek
                                             | otherwise = solvedNext
                                        where vertices = generateAllNodes (tryWithForTable x s 1) y_size x_size;
                                              vertex_head = trace ("head: ")head vertices;
                                              edges = generateAllEdges vertices y_size x_size s;
                                              edges1 = processAllEdges edges edges;
                                              graph = (vertices, edges1);
                                              check = checkConditions  np (tryWithForTable x s 1) y_size x_size;
                                              compact = isBoardCompact2 vertex_head graph (tryWithForTable x s 1) creek;
                                              value = compact && check;
											  solveNext p s = solve ps s (returnPossibleSolutions (head ps) s y_size x_size) y_size x_size (p:np) creek;
											  solvedNext = solveNext p (tryWithForTable x s 1)-}											  
{-								
solve :: [IntersectionDescription]-> [Int] -> [[Int]] ->Int->Int->[IntersectionDescription]->Creek->[Int]
--solve [] s _ _ _ _ = s
solve [p] s [] _ _ _ _ = s
solve [] s [[]] _ _ _ _ = s
solve (p:ps) s [[]] y_size x_size np creek  = solve ps s (returnPossibleSolutions (head ps) s y_size x_size) y_size x_size np creek
solve [p] s (x:xs) y_size x_size np creek | (value == True ) = (tryWithForTable x s 1) 
                                       | otherwise = solve [p] s xs y_size y_size np creek
                                        where vertices = generateAllNodes (tryWithForTable x s 1) y_size x_size;
                                              vertex_head = head vertices;
                                              edges = generateAllEdges vertices y_size x_size s;
                                              edges1 = processAllEdges edges edges;
                                              graph = (vertices, edges1);
                                              check = checkConditions  np (tryWithForTable x s 1) y_size x_size;
                                              compact = isBoardCompact2 vertex_head graph (tryWithForTable x s 1) creek;
                                              value = (trace ("s " ++ show s ++ " p " ++ show p ++ " check " ++ show check ++ " compact " ++ show compact ) ) compact && check

solve (p:ps) s (x:xs) y_size x_size np creek | (value == True ) = solve ps (tryWithForTable x s 1) (returnPossibleSolutions (head ps) s y_size x_size) y_size x_size (p:np) creek
                                       | otherwise = solve (p:ps) s xs y_size x_size np creek 
                                        where vertices = generateAllNodes (tryWithForTable x s 1) y_size x_size;
                                              vertex_head = head vertices;
                                              edges = generateAllEdges vertices y_size x_size s;
                                              edges1 = processAllEdges edges edges;
                                              graph = (vertices, edges1);
                                              check = checkConditions  np (tryWithForTable x s 1) y_size x_size;
                                              compact = isBoardCompact2 vertex_head graph (tryWithForTable x s 1) creek;
                                              value = (trace ("s " ++ show (tryWithForTable x s 1) ++ " p " ++ show p ++ " check " ++ show check ++ " compact " ++ show compact ++ " ps " ++ show ps) ) compact && check
											-}  
-- funkcja zwracająca wszystkie możliwe rozwiązania dla danego punktu w łamigłówce
returnPossibleSolutions:: IntersectionDescription ->[Int]->Int->Int->[[Int]]
returnPossibleSolutions ((a,b),c) board y_size x_size = combinations howManyOutOf4 (intersectionDescriptionstoi (getEmptiesin4 (squareAt  ((a,b),c) y_size x_size board  )) y_size)
                                                where howMany = howManyColoured (squareAt ((a,b),c) y_size x_size board);
                                                      howManyOutOf4 = c - howMany;
x = [0,1,1,2,0,1,1,2,0,0,0,0,0,0,0,0] 
solveIt = solve [ ((1, 0), 1), ((1, 2), 2), ((2, 3), 1)] x (returnPossibleSolutions ((1,0),1) x  4 4) 4 4 [] 

qwey = [0,1,1,2,0,1,1,2,0,1,0,0,0,0,0,0]
solveIt2 = solve [ ((3,3),1)] qwey (returnPossibleSolutions ((3,3),1) qwey  4 4) 4 4 [   ((1, 0), 1), ((1, 2), 2), ((2, 3), 1), ((3,3),1)] 

solveIt3 = solve [ ((1, 0), 1), ((1, 2), 2), ((2, 3), 1), ((3,3),1)] x (returnPossibleSolutions ((1,0),1) x  4 4) 4 4 [ ] 

-- funkcja sprawdzająca, czy dla każdego punktu spełnione są warunki (czy po każdej nowej iteracji algorytmu dla poprzednich punktów zgadza się ilość zamalowanych kratek)
checkConditions :: [IntersectionDescription]->[Int] ->Int->Int->Bool
checkConditions  [] _ _ _ = True
checkConditions  [((a,b),c)] board y_size x_size = if (howManyColoured (squareAt ((a,b),c) y_size x_size board) == c) then
                                                                True
                                                            else
                                                                False
checkConditions  (((a,b),c):ps) board y_size x_size | (howManyColoured (squareAt ((a,b),c) y_size x_size board) == c) = checkConditions  ps board y_size x_size
                                                             | otherwise = False
-- funkcja zwracająca listę warunków z Creek
getListFromCreek:: Creek -> [IntersectionDescription]
getListFromCreek (Creek (_,_) list) = list

-- funkcja do usuwania elementu z Creek
deleteElementFromCreek :: Creek -> IntersectionDescription -> Creek
deleteElementFromCreek (Creek (y,x) list ) ((a,b),c) = Creek (y,x) (deleteElementsFromPointList list ((a,b),c)) 

-- funkcja do usuwania IntersectionDescription z listy IntersectionDescription 												   
deleteElementsFromPointList :: [IntersectionDescription] -> IntersectionDescription -> [IntersectionDescription]
deleteElementsFromPointList [] _ = []
deleteElementsFromPointList ( ((a,b),c):xs ) ( (d,e),f ) | (a==d && b==e && c==f) = xs
                                                         | otherwise = ((a,b),c) : deleteElementsFromPointList xs ((d,e),f)
   
-- funkcja do obliczania początkowego stanu tablicy przy uwzględnieniu oczywistych przypadków np. gdy gdzieś wystąpi 4 lub 0 lub 2 na brzegu lub 1 w rogach
preprocess :: Creek -> [Int] -> [Int]
preprocess (Creek (_, _) []) xs = xs
preprocess (Creek (y_length, x_length) ( ((a,b),c) :xs ) ) board | (( (a==0 && b ==0) ||( a/=0 && b == 0) || (a/=0 && b==x_length) || (a==0 &&b/=0) || (a==y_length && b/=0)) && 
                                                                   (c == 0)) = preprocess (Creek (y_length, x_length) xs) 
                                                                   (tryWithForTable ( intersectionDescriptionstoi ( squareAt((a,b),c) 
                                                                    y_length x_length board) y_length ) board 2)
                                                                 | (( (a==0 && b == 0) ||( a/=0 && b == 0) || (a/=0 && b==x_length) || (a==0 &&b/=0) || (a==y_length && b/=0)) && c /= 0 &&
                                                                   (c == 2 || c == 3 || c == 4)) = preprocess (Creek(y_length, x_length)xs)
                                                                   (tryWithForTable ( intersectionDescriptionstoi ( squareAt((a,b),c) y_length x_length board) y_length ) board 1) 
                                                                 | ((a/=0 && b/=0) && (c == 0)) = preprocess (Creek (y_length, x_length) xs)
                                                                   (tryWithForTable ( intersectionDescriptionstoi ( squareAt((a,b),c) y_length x_length board) y_length ) board 2) 
                                                                 | ((a/=0 && b/=0) && (c == 4)) = preprocess (Creek(y_length,x_length) xs) 
                                                                   (tryWithForTable ( intersectionDescriptionstoi ( squareAt((a,b),c) y_length x_length board) y_length ) board 1) 
                                                                 | ((a==0 && b ==0) && (c == 1)) = preprocess (Creek(y_length,x_length) xs) 
                                                                   (tryWithForTable ( intersectionDescriptionstoi ( squareAt((a,b),c) y_length x_length board) y_length ) board 1)
                                                                 | otherwise = preprocess (Creek(y_length,x_length) xs) board 

-- funkcja usuwająca wszystkie elementy z danej listy z creek
deletePreprocessFromCreek :: Creek -> [IntersectionDescription] -> Creek
deletePreprocessFromCreek (Creek (y,x) list) [] = (Creek (y,x) list)
deletePreprocessFromCreek (Creek (y,x) list) (n:ns) = deletePreprocessFromCreek (deleteElementFromCreek  (Creek(y,x) list) n)   ns

-- stworzenie listy oczywistych przypadkow
createUsedPreprocessList :: Creek -> [IntersectionDescription]
createUsedPreprocessList (Creek (y_length,x_length) []) = []
createUsedPreprocessList (Creek (y_length, x_length) ( ((a,b),c) :xs ) ) | (( (a==0 && b ==0) ||( a/=0 && b == 0) || (a/=0 && b==x_length) || (a==0 &&b/=0) || (a==y_length && b/=0)) && 
                                                                           (c == 0 || c == 2 || c == 3 || c == 4)) ||
                                                                           ( ((a/=0 && b/=0) && (c == 0)) || ((a/=0 && b/=0) && (c == 4)) || ((a==0 && b ==0) && (c == 1)) )
                                                                            = ((a,b),c) : createUsedPreprocessList (Creek(y_length,x_length) xs)
                                                                         | otherwise = createUsedPreprocessList (Creek (y_length, x_length) xs)
--funkcja zwracająca przetworzony Creek czyli jak już się usunie te oczywiste warunki z niego
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

-- zamiana tablicy 2d na zwykla liste	
change2dInto1D ::[[Int]] -> [Int]
change2dInto1D  [] = []
change2dInto1D ((x:[]):ys) = x:change2dInto1D(ys)
change2dInto1D ((x:xs):ys) = x:change2dInto1D((xs):ys)

-- zamiana listy na tablice 2d
change1dinto2d :: Int -> [Int] -> [[Int]]
change1dinto2d _ [] = []
change1dinto2d n l
         | n > 0 = (take n l) : (change1dinto2d n (drop n l))
         | otherwise = error "Negative n"

-- drukowanie wyniku
printResult:: Int-> Int->[Int] -> String
printResult y_length x_length board = pPrint (changeIntsIntoChars( change2dInto1D ( (transpose (change1dinto2d y_length board))) )) x_length 

-- zamiana tablice intow na ladniejsze przedstawienie
changeIntsIntoChars:: [Int] -> [Int]
changeIntsIntoChars [] = []
changeIntsIntoChars (x:xs) | (x == 1) = 1:changeIntsIntoChars xs
                           | otherwise = 0:changeIntsIntoChars xs
-- funkcja, ktora trzeba wywolac by uruchomic program
totalSolution :: Creek -> String
totalSolution (Creek (y, x) list) = printResult y x (solve2 (generateNodesNOTDescribedByCreek table (Creek(y,x) list)) table (Creek (y, x) list))
                                  where creekList = getListFromCreek ( postprocessedCreek(Creek(y,x) list) );
                                        postTable = preprocess (Creek(y,x) list) (createListOfLength y x);
                                        firstPossibleSolution = {-(trace ("table" ++ show postTable) )-}returnPossibleSolutions ( head creekList) postTable y x;
										table = solve creekList postTable firstPossibleSolution  y x [] (Creek (y, x) list)
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


