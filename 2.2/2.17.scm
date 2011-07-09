(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(display (last-pair (list 23 72 149 34))) ;=> (34)
