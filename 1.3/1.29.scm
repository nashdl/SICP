(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term i)
    (cond ((= i 0) (y i))
          ((= i n) (y i))
          ((odd? i) (* 4 (y i)))
          (else (* 2 (y i)))))
  (define (iter i)
    (if (> i n)
        0.0
        (+ (term i)
           (iter (+ i 1)))))
  (* (/ h 3.0)
     (iter 0)))
  
(display (simpson cube 0 1 100))  ; 0.24999999999999992
(display "\n")
(display (simpson cube 0 1 1000)) ; 0.2500000000000002
