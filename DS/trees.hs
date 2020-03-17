module Trees where

data Tree v a = Leaf v a 
              | Node v (Tree v a) (Tree v a)
-- INTERPRETATION:
-- a Tree is annotated by the value v
-- The elements are stored in a    

type Size = Int 

leaf1 :: Tree Size Int
leaf1 = Leaf 1 5

leaf2 :: Tree Size Int 
leaf2 = Leaf 1 10

node1 :: Tree Size Int 
node1 = Node 2 leaf1 leaf2


toList :: Tree Size a -> [a]
toList (Leaf v a)   = [a]
toList (Node v l r) = toList l ++ toList r


-- tag returns the annotation of the tree
tag :: Tree Size a -> Size
tag (Leaf v a)   = v
tag (Node v l r) = v

-- first returns the leftmost element of the tree
first :: Tree v a-> a 
first (Leaf v a) = a 
first (Node _ l _) = first l 

-- constructs a leaf node with size 1
leaf :: a -> Tree Size a 
leaf a = Leaf 1 a

-- constructs a branching node with size = size of branches
node :: Tree Size a -> Tree Size a -> Tree Size a
node l r = Node (tag l + tag r) l r 

-- returns the nth element of the given tree
treeRef :: Tree Size a -> Int -> a 
treeRef (Leaf v a) 0 = a
treeRef (Leaf v a) n = error "number too big"
treeRef (Node v l r) n 
    | n < (tag l) = treeRef l n
    | otherwise   = treeRef r (n - tag l)

-- examples using `smart` constructors
leaf1' = leaf 5
leaf2' = leaf 10

node1' = node leaf1' leaf2' 

insert :: (Ord a) => a -> Tree Size a -> Tree Size a
insert a lf@(Leaf v a') = 
    if a < a' then node (leaf a) lf else node lf (leaf a)
-- insert a tr@(Node v l r) = 



