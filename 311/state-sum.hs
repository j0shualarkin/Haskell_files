module StateSum where
import Control.Monad.State

data TreeVal = Either Char Int 
        deriving (Show)
        
data Tree = Leaf TreeVal | Node TreeVal Tree Tree
        deriving (Show)

leaf :: TreeVal -> Tree 
leaf c = Leaf c 

node :: TreeVal -> Tree -> Tree -> Tree 
node c l r = Node c l r 

tree1 = leaf (Left 'c')

tree2 = leaf (Left 'o')

tree3 = node 
          (Left 'd') 
          tree1 
          tree2 

tree4 = node 
          (Left 'o') 
          (leaf (Left 'a'))
          (node
            (Left 'o') 
            (leaf (Left 't'))
            (node 
              (Left 'o') 
              (leaf (Left 'e'))
              (node 
                (Left 'm')
                (leaf (Left 'f'))
                (leaf (Left 'o')))))

tveq :: Char -> TreeVal -> Bool
sym `tveq` (Left ch) = sym == ch 
sym `tveq` (Right n) = False 

inc :: TreeVal -> TreeVal
inc (Right n) = (Right (n+1))
inc tr        = tr

replaceWithCount :: Char -> Tree -> State TreeVal Tree
replaceWithCount sym (Leaf char)
        | sym `tveq` char = do 
                                count <- (get) 
                                put (inc count)
                                return (leaf (inc count))
replaceWithCount sym (Node char tr1 tr2)
        | sym `tveq` char = do 
                              count <- (get)
                              put (inc count)
                              left <- replaceWithCount sym tr1 
                              right <- replaceWithCount sym tr2 
                              return (node count left right)
        | otherwise = do 
                        left <- replaceWithCount sym tr1
                        right <- replaceWithCount sym tr2 
                        return (node char left right)

