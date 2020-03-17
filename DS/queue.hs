{- ==============================================
|| Joshua Larkin -- joslarki
|| P515 -- Project 1
|| An implementation of the Queue data structure
   =============================================== -}

module Queue where
import Test.HUnit

-- invariant: if the front of the queue is empty then the whole queue is empty
data Queue = Queue { f :: [Int], r :: [Int] } deriving (Show, Eq)

-- smart constructor
queue :: [Int] -> [Int] -> Queue
queue [] r = Queue (reverse r) []
queue f r = Queue f r

-- adds an element to the queue, specifically the back 
enqueue :: Int -> Queue -> Queue
enqueue x (Queue f r) = queue f (x : r)

-- returns the element at the front of the queue
dequeue :: Queue -> (Maybe Int)
dequeue (Queue (x : f) r) = Just x
dequeue (Queue  []  r ) = Nothing

-- returns the queue with the exception of the front element
rest :: Queue -> (Maybe Queue)
rest (Queue (x : f) r) = Just (queue f r)
rest (Queue [] r) = Nothing

-- returns true iff the queue is empty
isEmpty :: Queue -> Bool
isEmpty (Queue [] _ ) = True
isEmpty _ = False  

-- ==========================
-- ||   Begin test suite   ||
-- ==========================

-- Because I'm lazy
mtq = queue [] []
pop = dequeue
push = enqueue
check = runTestTT

test1 = TestCase (assertEqual "pop after push" 
    (Just 4) (pop (push 4 mtq)))

test2 = TestCase (assertEqual "can't pop from empty queue" 
    Nothing (pop mtq))

test3 = TestCase (assertEqual "push 1, 2, 3, then pop for 1" 
    (Just 1) (pop (push 3 (push 2 (push 1 mtq)))))

test4 = TestCase (assertEqual "queue after popping 1"
    (Just (Queue [2,3] [])) (rest (push 3 (push 2 (push 1 mtq)))))

test5 = TestCase (assertEqual "queue after two pushes"
    (Queue [1] [2]) (push 2 (push 1 mtq)))

test15 = TestCase (assertEqual "queue above after `popping`"
    (Just (Queue [2] []))
    (rest (push 2 (push 1 mtq))))

test16 = TestCase (assertEqual "queue above after another call to rest"
    (Just (Queue [] []))
    (do q <- (rest (push 2 (push 1 mtq)))
        q' <- (rest q)
        return q'))

test6 = TestCase (assertEqual "queue after one more"
    (Queue [1] [3,2]) (push 3 (Queue [1] [2])))

test7 = TestCase (assertEqual "queue from test4 with two new elements"
    (Queue [2,3] [6,7]) (push 6 (push 7 (Queue [2,3] []))))

test8 = TestCase (assertEqual "queue restructures after two `rest`s"
    (Just (Queue [7,6] [])) 
    (do q <- (rest (Queue [2,3] [6,7]))
        q' <- (rest q)
        return q'))

test9 = TestCase (assertEqual "queue maintains FIFO after restructuring"
    (Just 7)
    (do q <- (rest (push 16 (push 7 (push 1 (push 33 mtq)))))
        q' <- (rest q)
        n <- (pop q')
        return n))

test10 = TestCase (assertEqual 
    "isEmpty after tail on 2 elt queue (e.g. (Q [12] [33]))"
    (Just False) 
    (do q <- (rest (push 33 (push 12 mtq)))
        return (isEmpty q)))

test11 = TestCase (assertEqual 
    "isEmpty after tail on 1 elt queue"
    (Just True) 
    (do q <- (rest (push 33 mtq))
        return (isEmpty q)))

test12 = TestCase (assertEqual 
    "tail on empty returns nothing"
    Nothing
    (do q <- (rest mtq)
        return q))

test13 = TestCase (assertEqual 
    "isEmpty on empty"
    True 
    (isEmpty mtq))

test14 = TestCase (assertEqual
    "isEmpty on not-empty"
    False 
    (isEmpty (push 4 mtq)))


tests = TestList ["test1" ~: test1, "test2" ~: test2, "test3" ~: test3,
                  "test4" ~: test4, "test5" ~: test5, "test6" ~: test6,
                  "test7" ~: test7, "test8" ~: test8, "test9" ~: test9,
                  "test10" ~: test10, "test11" ~: test11, "test12" ~: test12,
                  "test13" ~: test13, "test14" ~: test14, "test15" ~: test15,
                  "test16" ~: test16]
