
#lang racket

(define (sum-recur term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)))) 

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
      
(define (cube x)
  (* x x x))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x ( + x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
 
(define (simpsons-integral f a b n)
  (define (factor k)
    (if (< 0 k n)
        (if (even? k) 2 4)
        1))
  (let ((h (/ (- b a) n)))
    (define (y k) 
      (* (factor k) (f (+ a (* k h)))))
    (* (/ h 3) (sum y 0 inc n))))
    