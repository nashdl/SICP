; Recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (inc n) (+ n 1))

(define (identity n) n)

(display (sum identity 1 inc 4))
(display "\n")
(display (product identity 1 inc 4))
(display "\n")

; Iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(display (sum identity 1 inc 4))
(display "\n")
(display (product identity 1 inc 4))
