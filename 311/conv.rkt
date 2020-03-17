#lang racket

#|
write a  program that rewrites λ-calculus expressions
in racket syntax to the 311-haskell language
|#


(define e1
  '(App (App 
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
       (Num 5)))

(define e2
  '(Lam "x" 
        (Lam "y" (Var "x"))))

(define e3
  '(Lam "x" (Lam "y"
                 (Var "y"))))
