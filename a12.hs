import Prelude hiding (Word)



data PTree = End | Node {letter :: Char, forest :: PForest}

instance Show PTree where -- Just to make printing PTree pretty
    show End = "end"
    show (Node a f) = "(node " ++ (show a) ++ " (" ++ (concat $ (map show f)) ++ "))"

type PForest = [PTree] -- A Non empty list of PTree
type Word = String

ptree1 = End
ptree2 = Node 'a' [End]
ptree3 = Node 'o' [(Node 'n' [End, 
                              (Node 'e' [End])]),
                   (Node 'f' [End, 
                               (Node 'f' [End]),
                               (Node 't' [End])]),
                   (Node 'r' [End])
                   ]
pforest1 = [ptree3]
pforest2 = [ptree1,ptree2,ptree3]

srtd_pt3 = Node 'o' [(Node 'f' [End, ])]
rev_forest2 = [ptree3,ptree2,ptree1]


-- processTree End = ...
-- processTree n = ...  (letter n) ... (process-forest (forest n))

-- processForest [pt] = process-tree pt
-- processTree (x:xs) = ...  (process-tree x) ... (process-forest xs)

treeSize :: PTree -> Int
-- Counts the number of End in the given PTree
treeSize End = 1
treeSize (Node l f) = forestSize f

forestSize :: PForest -> Int
-- Counts the number of End in the given PForest
forestSize [pt] = treeSize pt
forestSize (pt:pf) = treeSize pt + forestSize pf


wordInTree :: Word -> PTree -> Bool
-- Check if the given Word is in the given PTree
wordInTree w End = null w
wordInTree (l:w) (Node a f) = if l == a then wordInForest w f else False

wordInForest:: Word -> PForest -> Bool
-- Check if the given Word is in the given PForest
wordInForest w [pt] = wordInTree w pt
wordInForest w (pt:pf) = wordInTree w pt || wordInForest w pf


treeToList :: PTree -> [Word]
-- Returns a list of all Words in the given PTree
treeToList End = [""]
treeToList (Node l f) = map ([l] ++) (treeToForest f)

treeToForest :: PForest -> [Word]
-- Returns a list of all Words in the given PForest
treeToForest [pt] = treeToList pt
treeToForest (pt:pf) = treeToList pt ++ treeToForest pf


wordToTree :: Word -> PTree
-- Converts the given Word to a PTree
wordToTree w = foldr (\a n-> Node a [n]) End w


addToTree :: Word -> PTree -> PTree
-- Adds the given Word to the given PTree (Assuming that first letter match and PTree is a Node)
addToTree (a:w) (Node l f) = Node l (addToForest w f)

addToForest :: Word -> PForest -> PForest
-- Adds the given Word to the given PForest
addToForest w f@[pt] = 
    case w of [] -> case pt of End -> f
                               otherwise -> End:f
              (a:xs) -> case pt of End -> End:[(wordToTree w)]
                                   (Node l f) -> if l == a then [addToTree w pt] else (pt:[wordToTree w])
addToForest w f@(pt:pf) =
    case w of [] -> case pt of End -> f
                               otherwise -> pt:(addToForest w pf)
              (a:xs) -> case pt of End -> pt:(addToForest w pf)
                                   (Node l f) -> if l == a then (addToTree w pt):pf else pt:(addToForest w pf)
                           

{--}

-- alphabetize sorts all the Nodes in a PrefixForest alphabetically
-- according to their Letter (ascending), and puts all Ends first
-- note: End < N where N is any node

alphabetize :: PForest -> PForest

alphabetize [tree] = 
    case tree of End -> [End]
                 otherwise -> End:[]

alphabetiez (tree:more) = more





{-
from init-file.hs 

-- haskell file

main = mapM print --((concat . map (\x -> (concat (map (\y -> [(x, y)]) ['a'..'z'])))) [1..10])
                  --[(a,b) | a <- [1..10] , b <- ['a' .. 'z']]
                  --([1..10] >>= (\a -> (['a'..'z'] >>= (\b -> return (a,b)))))
                  --((>>=) [1..10] (\a -> ((>>=) ['a'..'z'] (\b -> return (a,b)))))
                  (do
                    a <- [1..10]
                    b <- ['a'..'z']
                    return (a,b))
                    
-}