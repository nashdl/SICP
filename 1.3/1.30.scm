(load "1.29.scm")

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0.0))

(display (simpson cube 0 1 100))  ; 0.25
(display "\n")
(display (simpson cube 0 1 1000)) ; 0.25000000000000006
