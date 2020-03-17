module Lex where 

import Prelude hiding (lex, lookup)


data Expr = Var String | Lam String Expr | App Expr Expr 
data Lexd = Varx Int | Lamx Lexd | Appx Lexd Lexd
    deriving (Show)
type LexEnv = [(String,Int)]

lookup :: String -> LexEnv -> Int
lookup id [] = -1
lookup id ((x,n):xs) 
    | id == x = n 
    | otherwise = lookup id xs 

update :: String -> LexEnv -> LexEnv 
update x env = (x,0):(updateEnv env)
    where updateEnv xs = foldr (\pr ans -> ((fst pr),((snd pr)+1)):ans) [] xs


lex :: Expr -> LexEnv -> Lexd
lex (Var x)   env   = Varx (lookup x env)
lex (Lam x b) env   = Lamx (lex b (update x env))
lex (App f a) env   = (Appx (lex f env) (lex a env))

-- (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
lc1 = lex (Lam "y" (App (Lam "x" (App (Var "x") (Var "y")))
                        (Lam "c" (Lam "d" (App (Var "y") (Var "c")))))) []

lex1 = Lamx (Appx (Lamx (Appx (Varx 0) (Varx 1)))
                  (Lamx (Lamx (Appx (Varx 2) (Varx 1)))))


lc2 = 
    lex (Lam "a"
          (Lam "b"
            (Lam "c"
              (Lam "a"
                (Lam "b"
                  (Lam "d"
                    (Lam "a"
                      (Lam "e"
                        (App (App (App (App (App (Var "a") (Var "b")) (Var "c")) (Var "d")) (Var "e")) (Var "a")))))))))) []