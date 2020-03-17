module Stacks where

    class Stack a where 
        isEmpty :: Stack a -> Bool
        empty :: Stack a 

        cons :: a -> Stack a -> Stack a 
        first :: Stack a -> a
        rest :: Stack a -> Stack a
