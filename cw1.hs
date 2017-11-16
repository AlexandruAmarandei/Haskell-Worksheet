import Data.Char
import Data.List



-- Ex 1
subtotal :: Num a => [a] -> [a]
aux [] a = []
aux (x:xs) a = (a + x) : aux xs (a+x)
subtotal x = aux x 0


--Ex 2
countOccurencesBetween:: Int -> Int -> [Int] -> Int
countOccurencesBetween st fn xs = length[x|x<-xs, x <= fn, x >= st]

countOccurences :: Int -> Int -> [Int] -> [Int]
countOccurences t n xs 
    | t*n > maximum xs = []
    | otherwise = countOccurencesBetween (t*n) ((t+1)*n-1) xs : countOccurences (t+1) n xs
    
histogram :: Int -> [Int] -> [Int]
histogram c xs = countOccurences 0 c xs

--Ex 3
getSumOfMarks:: [Char] -> Int
getSumOfMarks []  = 0
getSumOfMarks (x:xs) 
          | x == 'A' = getSumOfMarks xs + 48
          | x == 'B' = getSumOfMarks xs + 40
          | x == 'C' = getSumOfMarks xs + 32
          | x == 'D' = getSumOfMarks xs + 24
          | x == 'E' = getSumOfMarks xs + 16
          | x == 'F' = getSumOfMarks xs + 8
          | x == '*' = getSumOfMarks xs + 8
          
meetsOffer :: [Char] -> Int -> Bool
meetsOffer [] 0 = True
meetsOffer [] _ = False
meetsOffer xs offer = getSumOfMarks(xs) >= offer

--Ex 4
data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving Show

ascendingList :: (Ord a) => [a] -> Bool
ascendingList [] = True
ascendingList [x] = True
ascendingList (x:y:xs) =  x<y && ascendingList (y:xs)

equalList :: (Ord a) => [a] -> Bool
equalList [] = True
equalList [x] = True
equalList (x:y:xs) = x==y && equalList (y:xs)

descendingList :: (Ord a) => [a] -> Bool
descendingList [] = True
descendingList [x] = True
descendingList (x:y:xs) = x>y && descendingList (y:xs)

aeList :: (Ord a) => [a] -> Bool
aeList [] = True
aeList [x] = True
aeList (x:y:xs) = (x<y || x==y) && aeList (y:xs)

deList :: (Ord a) => [a] -> Bool
deList [] = True
deList [x] = True
deList (x:y:xs) = (x>y || x==y) && deList (y:xs)

sortType :: Ord a => [a] -> TypeOfSort
sortType [] = Ascending
sortType [x] = Ascending
sortType (xs) | ascendingList(xs) = Ascending
              | equalList(xs) = Constant
              | descendingList(xs) = Descending
              | aeList(xs) = NonDescending
              | deList (xs) = NonAscending
              | otherwise   = NotSorted
 
 
--Ex 5
rpcalc :: [Char]-> Int
rpcalc expr = head ( foldl foldStack [] expr)
    where foldStack (x:y:xs) '+' = (y + x):xs
          foldStack (x:y:xs) '-' = (y - x):xs
          foldStack (x:y:xs) '*' = (y * x):xs
          foldStack (x:y:xs) '/' = (y `div` x):xs
          foldStack xs nextNumber = ( ord nextNumber - ord '0'):xs

          
--Ex 6
neighboursDistances :: (Floating a, Ord a) => (a,a) -> [(a,a)] -> [(a,a,a)]
neighboursDistances (a, b) xs = [(x,y,sqrt((a-x)**2 + (b-y)**2))| (x,y) <- xs ]

mergeSortM :: (Floating a, Ord a) => [(a,a,a)] -> [(a,a,a)] -> [(a,a,a)]
mergeSortM [] xs = xs
mergeSortM xs [] = xs
mergeSortM ((px,py,x):xs) ((ppx,ppy,y):ys) 
    | (x<=y) = (px, py, x):mergeSortM xs ((ppx,ppy,y):ys)
    | otherwise = (ppx, ppy, y):mergeSortM ((px,py,x):xs) ys
                         
mergeSortS :: (Floating a, Ord a) => [(a,a,a)] -> ([(a,a,a)], [(a,a,a)])
mergeSortS xs = (take l xs, drop l xs)
    where l = length xs `div` 2

mergeSort :: (Floating a, Ord a) => [(a,a,a)] -> [(a,a,a)]
mergeSort xs |(length xs) > 1 = mergeSortM (mergeSort ls) (mergeSort rs)
             | otherwise = xs
    where (ls, rs) = mergeSortS xs
    
neighboursSorted :: (Floating a, Ord a) => [(a,a,a)] -> [(a,a)]
neighboursSorted xs = [(a,b) | (a,b,c) <- xs]

neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]

neighbours k p ns 
    | length ns > k = take k (neighboursSorted (mergeSort( neighboursDistances p ns )))
    | otherwise = ns


-- Ex 7
data SearchTree = Node SearchTree Int SearchTree | Leaf Int deriving Show

treeSize :: SearchTree  -> Int
treeSize (Leaf x) = 1
treeSize (Node lt val rt) = treeSize lt + treeSize rt + 1

balancedNodes :: SearchTree -> Bool
balancedNodes (Leaf _) = True
balancedNodes (Node lt val rt) 
            | abs(treeSize lt - treeSize rt) <=1  && balanced lt && balanced rt = True
            | otherwise = False
            
getLeftMostLeaf :: SearchTree ->  Int
getLeftMostLeaf (Leaf x) = x 
getLeftMostLeaf (Node lt val rt) = getLeftMostLeaf lt    
getRightMostLeaf :: SearchTree ->  Int
getRightMostLeaf (Leaf x) = x 
getRightMostLeaf (Node lt val rt) = getRightMostLeaf rt        

ascendingTree :: SearchTree -> Bool
ascendingTree (Leaf _ ) = True
ascendingTree (Node lt val rt)
    | ascendingTree lt && ascendingTree rt && (val > getRightMostLeaf lt) && (val < getLeftMostLeaf rt) = True  
    | otherwise = False
type Boolean = Bool
balanced ::  SearchTree -> Boolean
balanced x = (ascendingTree x) && (balancedNodes x)


--Ex 8
newtonRootNextGuess :: (Double, Double) -> (Double,Double)
newtonRootNextGuess (guess,d) = (((guess + d / guess) / 2), d)

newtonRootSequence :: Double -> [(Double,Double)]
newtonRootSequence x = iterate (newtonRootNextGuess )(x,x) 

newtonRootAccept :: Double -> Double -> Double ->Double -> Bool
newtonRootAccept  d ep guess1 guess2 = ( (abs(guess1 - guess2)) < ep && d /= guess1 && d/=guess2)
   

findnewtonRootFromList :: Double-> Double -> [(Double,Double)]  -> Double
findnewtonRootFromList d ep ((g1, d1):(g2, d2):xs) 
    | newtonRootAccept d ep g1 g2 == True  = g2
    | otherwise = findnewtonRootFromList d ep ((g2,d2):xs)

newtonRoot:: Double -> Double -> Double 
newtonRoot d ep =  findnewtonRootFromList d ep (newtonRootSequence d)


--Ex 9
repeatHO :: Int -> Int -> [Int]
repeatHO x 0 = [ ]
repeatHO x y = x : repeatHO x (y-1)
hyperOperator :: Int -> Int -> Int -> Int
hyperOperator s x y | s == 1 = x + y
                    | s > 1 = foldl1 (flip(hyperOperator (s-1))) (repeatHO  x y) 
                    
                    
--Ex 10
binaryRepresentation :: Int -> [Int]  
binaryRepresentation 0 = [0]
binaryRepresentation 1 = [1]
binaryRepresentation x = x `mod` 2 : binaryRepresentation (x `div` 2)

characterCode :: Char -> [Int]
characterCode c = ex10br5 
    where ex10br = binaryRepresentation ( ord c) 
          ex10br2 | sum ex10br `mod` 2 == 0 = 0:ex10br     
                  | otherwise = 1:ex10br
          ex10br3 = reverse ex10br2  
          ex10br4 = repeatHO 0 (9 - (length ex10br3)) 
          ex10br5 = ex10br4 ++ ex10br3
encode :: String -> [Int]    
encode [] = []
encode (x:xs) = (characterCode x) ++ encode (xs)  
 
-- Ex 11
--My solution will not display the CHARACTERS that have an uneven parity
characterSum :: [Int] -> Int -> Int
characterSum [] _ = 0
characterSum x 0 = (last x)
characterSum (x:xs) y | y >0 = x*(2^(y-1)) + characterSum xs (y-1) 
                      | otherwise  = 0
codeToCharacter :: [Int] -> [Char]
codeToCharacter x 
        | sum x `mod` 2 == 1 = ""
        | otherwise = [chr(characterSum (take 8 x) 8)]

--If finding one wrong character means that we should return the whole string as "" then 
--we use one more binding by checking if every character has accepted parity    
checkForParity:: [Int] -> Bool
checkForParity [] = True
checkForParity x = (sum (take 9 x) `mod` 2 == 0) && (checkForParity (drop 9 x))       
decode:: [Int] -> String
decode x | length x `mod` 9 > 0 = ""
         | length x == 0 = ""
-- Uncomment next line to return "" in the case from above
--         | checkForParity x == False = ""
         | otherwise =codeToCharacter (take 9 x) ++ decode (drop 9 x)

         

-- Ex 12
nextSolution :: [Int] -> [(Int,[Int])] -> [(Int, [Int])]
nextSolution _ [] = []
nextSolution coinValues ((currentSum,currentRoad):ourSums) = [(currentSum-currentValue, currentRoad ++ [currentValue]) | currentValue <- coinValues] ++ nextSolution coinValues ourSums

removeUnnecesarrySol :: [(Int, [Int])] -> [(Int, [Int])]
removeUnnecesarrySol [] = []
removeUnnecesarrySol ((sum, road):sums) 
    | sum < 0 = removeUnnecesarrySol sums
    | otherwise = (sum, road) : removeUnnecesarrySol sums 
    
getRestSolution :: [Int] -> [(Int,[Int])] -> [(Int,[Int])]
getRestSolution denoms sums = nub $ removeUnnecesarrySol $ nextSolution denoms sums

findSol :: [Int] -> [(Int,[Int])] ->  [Int]
findSol _ [] = []
findSol denoms ((sum, path):posSol)
    | sum == 0 = path ++ [-2]
    | sum > 0  = findSol denoms (getRestSolution denoms [(sum, path)] ) ++ findSol denoms posSol

getFirstSol :: [Int] -> [Int]
getFirstSol [] = [-1]
getFirstSol (x:xs) 
    | x == -2 = []
    | otherwise = x: getFirstSol xs

frequency :: [Int] -> [Int] -> [Int]
frequency [] _   = []
frequency x [-1] = repeatHO (-1) (length x)
frequency (denom:denoms) sol = [(length. filter (==denom)) sol] ++ frequency denoms sol

makeChange :: Int -> [Int] -> [Int]
makeChange _ [] = []
makeChange sum denoms 
    |sum == 0 = repeatHO (0)  (length denoms)
    |otherwise = frequency  denoms (getFirstSol $ findSol denoms [(sum, [])])
    
-- Ex13
-- Seeing in the example that 7 [] is considered 0 in base 7
-- Then we accept starting elements such as x [] (0 in base x)
getNextVect :: Int -> [Int] -> Int -> [Int]
getNextVect _ [] _ = []
getNextVect _ [0] _ = [0]
getNextVect base (number:vector) diff 
    | number - diff < 0 = base - 1 : getNextVect base vector 1
    | otherwise = (number - diff) : getNextVect base vector 0
eliminateZeroes :: [Int] -> [Int]
eliminateZeroes [] = []
eliminateZeroes (x:xs) 
    | x == 0 = eliminateZeroes xs
    | otherwise = x:xs
getNextGood :: Int -> [Int] -> [Int]
getNextGood base vect =  reverse $ eliminateZeroes  $ reverse $ getNextVect base vect (1)

checkSequence ::(Int, [Int]) -> Bool
checkSequence (_, []) = True
checkSequence (x, y:ys) 
    | y >= 0 && y < x = True && checkSequence (x, ys)
    | otherwise = False
goodsteinSequence :: (Int, [Int]) -> [(Int, [Int])]
goodsteinSequence (x, []) = [(x, [])]
goodsteinSequence (base, number) 
    | checkSequence (base, number) == True = (base, number) : goodsteinSequence ((base+1), (getNextGood (base+1) number)) 
    | otherwise = []


-- ex14
-- Taken from TextBook 'Graham Hutton, Programming in Haskell chapter 8' \/\/
data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop
    
type Subst = Assoc Char Bool
type Assoc k v = [(k,v)] 
bookfind :: Eq k  =>  k ->  Assoc k v -> v
bookfind k t  = head [v | (k', v) <-  t, k ==k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var  x) = bookfind x s
eval s (Not b)  = not ( eval s b)
eval s (And q p) = eval s p && eval  s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) =  [] 
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)
    
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False: ) bss ++ map (True:) bss
    where bss  = bools (n-1)

rmdups :: Eq a =>  [a] -> [a]
rmdups [] = [] 
rmdups (x:xs) = x:filter (/=x) (rmdups xs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

--Taken from TextBook 'Graham Hutton, Programming in Haskell chapter 8' /\/\
checkIfTrue :: Prop -> [Subst] -> [Subst]
checkIfTrue _ [] = []
checkIfTrue p (x:xs) 
    | eval x p == True = [x]
    | otherwise = checkIfTrue p xs

isSat :: Prop -> [Subst]
isSat x = checkIfTrue x (substs x)


--Ex15
getW :: Int -> Int
getW z = floor ((sqrt(fromIntegral((z*8)+1))-1) / 2) 
getT :: Int -> Int
getT w = (((w+1)*w) `div` 2)
getY :: Int -> Int -> Int
getY t z= (z - t)  
getX :: Int -> Int -> Int
getX w y = (w - y)

getCantorPair:: Int -> (Int, Int)
getCantorPair z = ((getX w y), y)
    where w = getW z
          t = getT w
          y = getY t z
computeCantorPair:: (Int, Int) -> Int
computeCantorPair (x,y) = y +(((x+y)*(x+y+1)) `div` 2)
  
isCantorPair :: Int -> Bool
isCantorPair z = (z == computeCantorPair (x,y)  && x ==computeCantorPair(x2,x2) && (x2+y2) == y)
    where (x,y) = getCantorPair(z)
          (x2,y2) = getCantorPair(x)
    
        