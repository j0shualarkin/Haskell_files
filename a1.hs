

{-
    A1 from C311
    Joshua Larkin
-}


add1 :: Int -> Int
add1 n = n + 1

sub1 :: Int -> Int
sub1 0 = 0
sub1 n = n - 1


checkEqual :: Eq a => a -> a -> Bool
checkEqual a b = a == b


-- ex1 
-- countdown 5 = [5..0]
countdown :: Int -> [Int]
countdown 0 = [0]
countdown n = n : countdown (n - 1)

-- ex2
-- insertR 'x' 'y' "xzzxyx" = "xyzzxyyxy"
insertR :: Eq a => a -> a -> [a] -> [a]
insertR s1 s2 (x:xs) = foldr (\i -> \ans -> if (s1 == i) then (s1 : (s2 : ans)) else (i : ans)) [] (x:xs)

-- ex3
-- remv 'x' "xyzx" = "yzx"
-- remv 'y' "xyzyx" = "xzyx"
remv :: Eq a => a -> [a] -> [a]
remv _ [] = []
remv s (x:xs) = if (s == x) then xs else x : remv s xs

-- ex4
-- indexof 'x' "xyzxx" = 0
-- indexof 'x' "yzxx" = 2
indexof :: Eq a => a -> [a] -> Int
indexof s (x:xs) = if (x == s) then 0 else 1 + indexof s xs



-- ex5 
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p [] = []
myfilter p (x:xs) = if (p x) then x : (myfilter p xs) else (myfilter p xs)

filter_fr :: (a -> Bool) -> [a] -> [a]
filter_fr p xs = foldr (\i -> \ans -> (if (p i) then i : ans else ans)) [] xs



-- checkEqual (myfilter (\x -> not $ even x) [0..10]) [1,3,5,7,9]

{-
-- ex6 
zipJ :: Eq a => [a] -> [a] -> [[a]]
zipJ xs [] = []
zipJ [] ys = []
zipJ (x:xs) (y:ys) = (x:y) : (zipJ xs ys)
-}

-- ex7 
myMap :: (x -> y) -> [x] -> [y]
myMap op [] = []
myMap op (x:xs) = (op x) : myMap op xs

map_fr :: (x->y) -> [x] -> [y]
map_fr op xs = foldr (\i -> \ans -> (op i) : ans) [] xs


-- ex8
myAppend :: [x] -> [x] -> [x]
myAppend [] ys = ys
myAppend (x:xs) ys = x : (myAppend xs ys)

append_fr :: [x] -> [x] -> [x]
append_fr xs ys = foldr (:) ys xs


-- ex9
myReverse :: [x] -> [x]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) (x:[])

reverse_fr :: [x] -> [x]
reverse_fr xs = foldr (\i -> \ans -> (append_fr ans (i:[]))) [] xs


-- ex10
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)


-- ex11
memv :: Eq a => a -> [a] -> Maybe [a]
memv s [] = Nothing
memv s (x:xs) = if (s == x) then Just (x:xs) else memv s xs


-- ex12
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

finfbs :: Int -> [Int]
finfbs 0 = []
finfbs 1 = [1]
finfbs n = (finfbs (n - 1)) ++ ((fib n):[])

zeroP :: Int -> Bool
zeroP 0 = True
zeroP _ = False


inf_fibs :: Int -> [Int]
inf_fibs n
    | zeroP n = []
    | zeroP $ sub1 n = [n]
    | otherwise      = (inf_fibs $ sub1 n) ++ ((fib n) : [])

------------------------------------------------
-- ex13
-- binary->natural
btn :: [Int] -> Int
btn [] = 0
btn (x:xs) = x + (2 * btn xs)

--

btn_step :: Int -> Int -> Int
btn_step b ans = b + (2 * ans)

btn_fr :: [Int] -> Int
btn_fr bits = foldr btn_step 0 bits
------------------------------------------------



-- ex14 minus
minus :: Int -> Int -> Int
minus m 0 = m
minus m n = sub1 (minus m (sub1 n))

-- ex15 div
{-
mydiv :: Int -> Int -> Maybe Int
mydiv m 0 = Nothing
mydiv m n = do  x <- Just x
                add1 (mydiv m (minus m n))
-}

myappendmap :: (x -> [x]) -> [x] -> [x]
myappendmap f [] = []
myappendmap f (x:xs) = myAppend (f x) (myappendmap f xs)

appendmap_fr :: (x -> [x]) -> [x] -> [x]
appendmap_fr f xs =
    foldr (\x -> \ans -> append_fr (f x) ans) [] xs



{-
setDiff :: [x] -> [x] -> [x]
setDiff [] ys = ys
setDiff (x:xs) ys = if (memv x ys) == Just then setDiff xs ys else x : (setDiff xs ys)
-}

-- take a list l, outputs all sublist of l; i.e. powerset
powerset ::  [a] -> [[a]]
powerset xs = foldr (\i -> \ans -> [i : x | x <- ans] ++ ans) [[]] xs


cartesian ::  [[a]] -> [[a]]
cartesian xs = 
    foldr (\x -> \ans -> 
        foldr (\almost -> \final -> [e : almost | e <- x] ++ final) [] ans) [[]] xs


collatz :: Int  -> [Int]
collatz n 
    | n == 1            = [1]
    | n `mod` 2 == 0    = n : collatz (n `div` 2)
    | otherwise         = n : collatz (1 + 3 * n)