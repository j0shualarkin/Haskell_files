module DynamicScope where

{- An Expr is one of 
    | Var 
    | Lam X Expr
    | if Expr Expr Expr
    | let Expr be Expr in Expr 
    | App Expr Expr 
-}

data Val = IntVal Integer 
    | Closure (Val -> Maybe Val) 
    | BoolVal Bool 
    | Clos String Expr 
    

data Expr = Var String 
    | Boolean Bool
    | Num Integer 
    | Lam String Expr 
    | App Expr Expr 
    | If Expr Expr Expr
    | Zerop Expr
    | Sub1 Expr
    | Times Expr Expr
    | Let Expr Expr Expr
        

type Env = String -> Maybe Val 

lookup' :: String -> Env -> Maybe Val 
lookup' x env = env x 

extend x var env = (\a -> if a == var then Just x else lookup' var env)

empty_env _ = Nothing

interp :: Expr -> Env -> Maybe Val 
interp (Var x) env = 
    lookup' x env 
interp (Num n) env = 
    Just (IntVal n)
interp (Boolean b) env = 
    Just (BoolVal b)
interp (Lam var body) env = 
    Just (Clos var body)
interp (Let (Var x) e body) env = 
    do 
        a <- (interp e env)
        interp body (extend a x env)
interp (If q c e) env = 
    do 
        (BoolVal b) <- interp q env 
        if b then interp c env else interp e env  
interp (Zerop n) env = 
    do 
        (IntVal n) <- interp n env 
        return (BoolVal (n == 0))
interp (Sub1 n) env = 
    do 
        (IntVal n) <- interp n env 
        return (IntVal (n - 1))
interp (Times m n) env = 
    do
        (IntVal m_val) <- interp m env 
        (IntVal n_val) <- interp n env 
        return (IntVal (n_val * m_val)) 
interp (App rator rand) env = 
    do 
        (Clos var body) <- interp rator env 
        val <- interp rand env 
        (interp body (extend val var env))


printf val = case val of Just (IntVal i)  -> show i  
                         Just (Closure _) -> "some function"
                         Just (BoolVal b) -> show b
                         Nothing          -> "oh, some error"

eval e = printf $ interp e empty_env

e1 = (App (App 
       (Lam "f" 
        (Lam "n" 
          (If (Zerop (Var "n"))
              (Num 1) 
              (Times (Var "n") 
                 (App (App (Var "f") (Var "f")) (Sub1 (Var "n")))))))
        (Lam "f" 
          (Lam "n" 
            (If (Zerop (Var "n"))
                (Num 1) 
                (Times (Var "n") 
                  (App (App (Var "f") (Var "f")) (Sub1 (Var "n"))))))))
             (Num 5))

e2 = (Lam "x" 
        (Lam "y" (Var "x")))
e3 = (Lam "x" (Lam "y" (Var "y")))

e4 = (App (App e2 (Num 10)) (Num 42))

test1 = 
    (Let (Var "x") (Num 2)
        (Let (Var "f") (Lam "e" (Var "x"))
            (Let (Var "x") (Num 5) 
                (App (Var "f") (Num 0)))))

test2 =
    (Let (Var "f")
         (Lam "x" (Times (Var "x") (Var "x")))
         (App (Var "f") (Num 5)))