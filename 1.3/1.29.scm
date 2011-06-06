(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (iter i)
    (if (= i n)
        (y i)
        (+ (* 4 (y (- i 1)))
           (* 2 (y i))
           (iter (+ i 2)))))
  (* (/ h 3.0)
     (+ (y 0)
        (iter 2))))
  
(display (simpson cube 0 1 100))  ; 0.23706268
(display "\n")
(display (simpson cube 0 1 1000)) ; 0.248670662668
