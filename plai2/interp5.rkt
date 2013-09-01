#lang plai-typed

;;;; stolen from:
;    0. plai book
;    1. http://www.eng.utah.edu/~cs5510/function+parse.rkt

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "refference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for l))]
    [multC (l r) (multC (subst what for l)
                        (subst what for l))]))
  
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(and (s-exp-list? s)
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? '+ (s-exp->symbol (first (s-exp->list s)))))
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(and (s-exp-list? s)
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? '* (s-exp->symbol (first (s-exp->list s)))))
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ; (double x)
    [(and (s-exp-list? s)
          (= 2 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s))))
     (appC (s-exp->symbol (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))
    
(define (parse-fundef [s : s-expression]) : FunDefC
  (cond
    [(and (s-exp-list? s)
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? 'define (s-exp->symbol (first (s-exp->list s))))
          (s-exp-list? (second (s-exp->list s)))
          (= 2 (length (s-exp->list (second (s-exp->list s)))))
          (s-exp-symbol? (first (s-exp->list (second (s-exp->list s)))))
          (s-exp-symbol? (second (s-exp->list (second (s-exp->list s))))))
     (fdC (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
          (s-exp->symbol (second (s-exp->list (second (s-exp->list s)))))
          (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local [(define fd (get-fundef f fds))]
                  (interp (subst (numC (interp a fds)) 
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(define double-def
  (parse-fundef '(define (double x) (+ x x))))
  
(define quadruple-def
  (parse-fundef '(define (quadruple-def x) (double (double x)))))

(test
 (parse-fundef '(define (double x) (+ x x)))
 (fdC 'double 'x (plusC (idC 'x) (idC 'x))))

(test 
 (parse-fundef '(define (quadruple x) (double (double x))))
 (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))

(test 
 (interp (parse '(double 9)) (list double-def))
 18)