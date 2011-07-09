(define (same-parity h . t)
  (define (same-parity-iter fn result l)
    (if (null? l)
        result
        (same-parity-iter
          fn
          (if (fn (car l))
              (append result (list (car l)))
              result)
          (cdr l))))
  (same-parity-iter 
    (if (even? h) even? odd?)
    (list h)
    t))

(display (same-parity 1 2 3 4 5 6 7)) ;=> (1 3 5 7)
(newline)
(display (same-parity 2 3 4 5 6 7)) ; => (2 4 6)
