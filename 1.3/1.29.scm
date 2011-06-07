(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0.0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term i)
    (cond ((= i 0) (y i))
          ((= i n) (y i))
          ((odd? i) (* 4 (y i)))
          (else (* 2 (y i)))))
  (* (/ h 3.0)
     (sum term 0 inc n)))
  
; (display (simpson cube 0 1 100))  ; 0.24999999999999992
; (display "\n")
; (display (simpson cube 0 1 1000)) ; 0.2500000000000002
