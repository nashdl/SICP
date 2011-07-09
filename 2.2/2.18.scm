(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l)) (list (car l)))))

(display (reverse (list 1 4 9 16 25))) ;=> (25 16 9 4 1)
