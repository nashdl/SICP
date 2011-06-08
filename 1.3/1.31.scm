;Iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity n) n)

(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))

; (display (factorial 4)) ; => 24

(define (approx-pi count)
  (define (num-term i)
    (- (+ i 2) (remainder i 2)))
  (define (denom-term i)
    (+ i 1 (remainder i 2)))
  (define (term i)
    (/ (num-term i) 1.0 (denom-term i)))
  (* (product term 1 inc count) 4))

(display (approx-pi 100)) ; => 3.1570301764551654

(display "\n")

;Recursive
(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a) (product term (next a) next b))))

(display (approx-pi 100)) ; => 3.1570301764551645
