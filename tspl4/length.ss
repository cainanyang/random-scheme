(define length
  (lambda (ls)
    (cond
     [(null? ls) 0]
     [else (+ 1 (length (cdr ls)))])))