(define sign
  (lambda (n)
    (cond
     [(> n 0) 1]
     [(> 0 n) -1]
     [else 0])))