#lang racket/base


;;
;; data Expr = Var String 
;;     | Boolean Bool
;;     | Num Integer 
;;     | Lam String Expr 
;;     | App Expr Expr 
;;     | If Expr Expr Expr
;;     | Zerop Expr
;;     | Sub1 Expr
;;     | Times Expr Expr
;;

(require racket/match
         rackunit)



(define (convert LC)
  (match LC
    [(? symbol? y)       `(Var   ,(symbol->string y))]
    [(? number? n)       `(Num   ,n)]
    [`(λ (,x) ,b)        `(Lam   ,(symbol->string x) ,(convert b))]
    [`(lambda (,x) ,b)   `(Lam   ,(symbol->string x) ,(convert b))]
    [`(if ,Q ,C ,E)      `(If    ,(convert Q) ,(convert C) ,(convert E))]
    [`(zero? ,n)         `(Zerop ,(convert n))]
    [`(sub1 ,n)          `(Sub1  ,(convert n))]
    [`(* ,m ,n)          `(Times ,(convert m) ,(convert n))]
    [`(let ([,x ,e]) ,b) `(Let   ,(convert x) ,(convert e) ,(convert b))]
    [`(,rator ,rand)     `(App   ,(convert rator) ,(convert rand))]))



(define fact5
  '(((λ (f)
    (λ (n)
      (if (zero? n) 1 (* n ((f f) (sub1 n))))))
   (λ (f)
     (λ (n)
       (if (zero? n) 1 (* n ((f f) (sub1 n))))))) 5))

(check-equal? (convert '(λ (x) x)) '(Lam "x" (Var "x")))
(define 5!.hs (convert fact5))

