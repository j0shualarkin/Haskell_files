


data Val = IntVal Integer | Closure String Expr Env | BoolVal Bool 
    deriving (Show) 

    -- data Var = Var String

data Expr = Var String 
    | Lam String Expr 
    | App Expr Expr 
    | Num Integer 
    | Let Expr Expr Expr
    | Boolean Bool
    | If Expr Expr Expr
    | Zerop Expr
    | Sub1 Expr
    | Times Expr Expr
    deriving (Show)

type Env = [(String, Val)] 


lookup' :: String -> Env -> Maybe Val 
lookup' _ [] = Nothing
lookup' x ((y,v):env) 
    | x == y    = Just v
    | otherwise = lookup' x env 

empty_env :: Env
empty_env = []

extend_env :: String -> Val -> Env -> Env 
extend_env x a env = (x,a) : env


interp :: Expr -> Env -> Maybe Val 
interp (Var x) env = lookup' x env 
interp (Num n) env = 
    return (IntVal n)
interp (Boolean b) env = 
    return (BoolVal b)
interp (Lam var body) env = 
    return (Closure var body env)
interp (Let (Var x) e b) env =
    do 
        a <- interp e env 
        interp b (extend_env x a env)
interp (If q c e) env = 
    do 
        (BoolVal b) <- interp q env 
        if b then interp c env else interp e env  
interp (Zerop n) env = 
    do 
        (IntVal n') <- interp n env 
        return (BoolVal (n' == 0))
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
        (Closure var body env_) <- interp rator env 
        arg  <- interp rand env 
        interp body ((var,arg) : env_)

eval exp = interp exp empty_env



e1 = (App (Lam "x" (Var "x")) (Num 10))
e2 = (App (App (Lam "x" (Lam "y" (Var "x"))) (Num 8)) (Num 5))

zero_test = (If (Zerop (Num 0)) (Num 32) (Num 1))
times_test = (Times (Num 2) (Num 43))
one_step = (App (Lam "n" (If (Zerop (Var "n")) (Num 1) 
    (Times (Var "n") (Num 24)))) (Num 0))
ind_step = (App (Lam "n" (If (Zerop (Var "n")) (Num 1) 
    (Times (Var "n") (Num 24)))) (Num 5))

simple = (App (Lam "f" (App (Var "f") (Num 2)))
    (Lam "n" (Times (Var "n") (Num 4))))

fo = (App (Lam "f" (Times (App (Var "f") (Num 2)) (App (Var "f") (Sub1 (Num 5)))))
          (Lam "n" (Times (Var "n") (Num 3))))

let1 = (Let (Var "x") (Num 6) (Times (Var "x") (Var "x")))

e3 = (App (App 
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
