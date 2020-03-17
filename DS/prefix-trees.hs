module PrefixTrees where
import Data.List
-- A PrefixTree is a 
-- End
-- Initial Letter Forest
data PrefixTree = End 
    | Initial String [PrefixTree]
    deriving Show


basetree :: PrefixTree 
basetree = End

tree1 :: PrefixTree 
tree1 = Initial "o" [(Initial "n" [End, (Initial "e" [End])]),
                     (Initial "f" [End, (Initial "f" [End]), 
                                        (Initial "t" [End])]),
                     (Initial "r" [End])]

tree2 :: PrefixTree 
tree2 = Initial "d" [(Initial "o" [End, (Initial "g" [End])]),
                     (Initial "r" [(Initial "a" [(Initial "f" [(Initial "t" [End])]),
                                                 (Initial "g" [End])])])]

tree3 :: PrefixTree 
tree3 = Initial "c" [(Initial "a" [(Initial "r" [End]),
                                   (Initial "t" [End])]),
                     (Initial "o" [(Initial "w" [End])])]


forest1 :: [PrefixTree]
forest1 = [tree2, tree1]
forest2 :: [PrefixTree]
forest2 = [tree3, tree2]

treeSize :: PrefixTree -> Int 
treeSize End = 1
treeSize (Initial _ forest) = forestSize forest 

forestSize :: [PrefixTree] -> Int 
forestSize = foldr (\i ans -> treeSize i + ans) 0

{-
forestSize [tr]         = treeSize tr
forestSize (tr:forest)  = treeSize tr + forestSize forest  
-}


treeList :: PrefixTree -> [String]
treeList End = [""]
treeList (Initial str forest) = [ str++word | word <- forestList forest]

forestList :: [PrefixTree] -> [String]
forestList forest = [ word | tr <- forest, word <- treeList tr] 

first :: String -> String 
first s = take 1 s

rest :: String -> String 
rest s = drop 1 s

wordToTree :: String -> PrefixTree 
wordToTree ""  = End
wordToTree str = Initial (first str) [(wordToTree (rest str))]

epsilon :: String -> Bool 
epsilon str = str == ""

wordInTree :: String -> PrefixTree -> Bool
wordInTree str End                     = epsilon str 
wordInTree str (Initial letter forest) 
    | (first str) == letter =  wordInForest (rest str) forest
    | otherwise             =  False

wordInForest :: String -> [PrefixTree] -> Bool 
wordInForest str forest = or [ (wordInTree str tr) | tr <- forest]


addToTree :: String -> PrefixTree -> PrefixTree
addToTree str (Initial s forest) = Initial s (addToForest (rest str) forest)

addToForest :: String -> [PrefixTree] -> [PrefixTree]
addToForest str [tr]         = addToSingleton str [tr]
addToForest str (End:forest) = End:(addToForest str forest)
addToForest str ((Initial letter initfst):forest) 
    | (first str) == letter  = (addToTree str (Initial letter initfst)):forest 
    | otherwise              = (Initial letter initfst):(addToForest str forest)

addToSingleton :: String -> [PrefixTree] -> [PrefixTree]
addToSingleton str [End]        = (Initial (first str) (addToForest (rest str) [End])):[End]
addToSingleton str [(Initial letter forest)]
        | (first str) == letter = [(addToTree str (Initial letter forest))]
        | otherwise             = (wordToTree str):[(Initial letter forest)]

explode :: [Char] -> String 
explode s = [ s' | s' <- s]

listForest :: [String] -> [PrefixTree]
listForest [word]   = [wordToTree word]
listForest (s:strs) = addToForest (explode s) (listForest strs)

treeComp :: PrefixTree -> PrefixTree -> Ordering
treeComp End _ = LT
treeComp _ End = GT
treeComp (Initial s1 f1) (Initial s2 f2)
  | s1 < s2   = LT
  | otherwise = GT

alphabetize :: [PrefixTree] -> [PrefixTree]
alphabetize forest = 
    [ case pt of End -> End
                  (Initial l pf) -> (Initial l (alphabetize pf))  | pt <- sortBy treeComp forest]
    




sorter = forestList . alphabetize

ptSort = forestList . alphabetize . listForest

list1 = ["dogs", "danq", "bbq", "quest", "soccer", "gatorade", "chapstick"]
