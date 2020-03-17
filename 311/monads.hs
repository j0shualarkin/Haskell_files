module Monads where 
import Control.Monad.Writer
import Control.Monad.State

remCountEvens :: [Int] -> State Int [Int]
remCountEvens [] = return []
remCountEvens (x:xs) 
    | even x = do 
                n <- get 
                put (n+1)
                remCountEvens xs 
    | otherwise = remCountEvens xs >>= (\d -> return (x:d))



findf :: (a -> Bool) -> [a] -> Maybe a
findf _ [] = Nothing
findf f (x:xs)
    | f x = Just x
    | otherwise = findf f xs

zeroP :: Int -> Bool
zeroP x = x == 0

test1 = findf zeroP [1..5]
-- Nothing
test2 = findf zeroP [1,2,0,3,4]
-- Just 0

partition :: [a] -> (a -> Bool) -> Writer [a] [a]
partition [] _      = return []
partition (x:xs) f 
    | f x           = tell [x] >> partition xs f 
    | otherwise     = partition xs f >>= (\d -> return (x:d))

partition1 = partition [1..5] even 
-- Writer [1,3,5] [2,4]
partition2 = partition [1..5] odd
-- Writer [2,4] [1,3,5]


power :: (Integral a) => a -> a -> a
power x 0 = 1
power x 1 = x
power x n 
    | odd n  = x * power x (n - 1)
    | even n = let nhalf = (n `div` 2) in 
                let y = power x nhalf in
                    y * y 


powerXpartials :: (Integral a) => a -> a -> Writer [a] a
powerXpartials _ 0 = return 1
powerXpartials x 1 = return x
powerXpartials x n 
    | odd n  = do 
                y <- powerXpartials x (n - 1) 
                tell [y]
                return (x * y)
    | even n = do 
                y <- powerXpartials x (n `div` 2)
                tell [y]
                return (y * y)


data Tree = Leafc Char 
    | Leafn Int
    | Nodec Char Tree Tree
    | Noden Int Tree Tree
    deriving (Show)

leafc :: Char -> Tree 
leafc c = Leafc c 

leafn :: Int -> Tree
leafn n = Leafn n

nodec:: Char -> Tree -> Tree -> Tree 
nodec c l r = Nodec c l r 

noden:: Int -> Tree -> Tree -> Tree 
noden n l r = Noden n l r 

tree1 = leafc 'c'

tree2 = leafc 'c'

tree3 = nodec 'd' tree1 tree2 

tree4 = nodec 
          'o'
          (leafc 'a')
          (nodec 'o'
            (leafc 't')
            (nodec 
              'o' 
              (leafc 'e')
              (nodec 
                'm'
                (leafc 'f') 
                (leafc 'o'))))



replaceWithCount :: Char -> Tree -> State Int Tree
replaceWithCount sym (Leafc char)
        | sym == char = do 
                          count <- (get) 
                          put (count+1)
                          return (leafn (count+1))
        | otherwise = return (leafc char)
replaceWithCount _ (Leafn n) = return (leafn n)
replaceWithCount sym (Nodec char tr1 tr2)
        | sym == char = do 
                          count <- (get)
                          put (count+1)
                          left <- replaceWithCount sym tr1 
                          right <- replaceWithCount sym tr2 
                          return (noden count left right)
        | otherwise = do 
                        left <- replaceWithCount sym tr1
                        right <- replaceWithCount sym tr2 
                        return (nodec char left right)
replaceWithCount sym (Noden n tr1 tr2) = 
    do 
      left <- replaceWithCount sym tr1
      right <- replaceWithCount sym tr2
      return (noden n left right)

replace sym tr s = runState (replaceWithCount sym tr) s

{-
reciprocal :: (Integral a) => a -> Maybe a
reciprocal 0 = Nothing
reciprocal n = Just (1 / n) 
-}

halve :: (Integral a) => a -> Writer [a] a
halve n 
  | even n    = return (n `div` 2)
  | otherwise = tell [n] >> return n 

sum_state :: Int -> State Int Int 
sum_state n = get >>= \s -> return (n+s)

sum_state' :: Int -> Int -> (Int,Int)
sum_state' n m = runState (sum_state n) m
