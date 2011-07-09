(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (how-many 2 (remove-factor 3 z)))

(define (cdr z)
  (how-many 3 (remove-factor 2 z)))

(define (how-many factor n)
  (if (= n 1)
      0
      (+ 1 (how-many factor (/ n factor)))))

(define (remove-factor factor n)
  (if (= (remainder n factor) 0)
      (remove-factor factor (/ n factor))
      n))

(display (car (cons 3 4)))
(newline)
(display (cdr (cons 3 4)))
