module Sorting where

list1 :: [Int]
list1 = [1,5,6,2,3,7,8,10,12,14,13]


-- ======================
-- insertion
-- ======================
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x ls@(y:ys) 
    | x <= y = x:ls 
    | otherwise = y:(insert x ys)

insertion :: (Ord a) => [a] -> [a]
insertion = foldr insert []

-- ======================
-- selection
-- ======================
delete :: (Eq a) => a -> [a] -> [a]
delete x = filter (x /=)

selectionHelper :: (Ord a) => [a] -> [a] -> [a]
selectionHelper [] sorted = sorted
selectionHelper ls sorted = 
    let m = maximum ls in 
        selectionHelper (delete m ls) (m:sorted)

selection :: (Ord a) => [a] -> [a]
selection xs = selectionHelper xs []

-- ======================
-- bubble
-- ======================
len :: [a] -> Int 
len = foldr (\_ ans -> (1 + ans)) 0

swap :: (Ord a) => [a] -> [a]
swap [x] = [x]
swap (x:(y:ys))
        | x > y     = y:(swap (x:ys))
        | otherwise = x:(swap (y:ys)) 

bub :: (Ord a) => Int -> [a] -> [a]
bub 0 xs = xs 
bub n xs = bub (n-1) (swap xs)


bubble :: (Ord a) => [a] -> [a]
bubble xs = bub (len xs) xs


-- ======================
-- quick
-- ======================
quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (x:xs) = (quick lhs) ++ [x] ++ (quick rhs)
        where lhs = filter (< x) xs
              rhs = filter (> x) xs

-- ======================
-- merge
-- ======================
combine :: (Ord a) => [a] -> [a] -> [a]
combine [] x = x 
combine x [] = x
combine ls1@(x:xs) ls2@(y:ys) 
        | x < y     = x:(combine xs ls2)
        | otherwise = y:(combine ls1 ys)

merge :: (Ord a) => [a] -> [a]
merge []  = []
merge [x] = [x]
merge ls  = combine (merge left) (merge right)
    where n       = len ls 
          split f = (f (n `div` 2) ls)
          left    = split take 
          right   = split drop

