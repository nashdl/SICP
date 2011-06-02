
#lang racket

(define (sum term a next b)
(if (> a b)
  0
  (+ (term a)
    (sum term (next a) next b)))) 

(define (cube x)
  (* x x x))

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

(define (even? x)
  (define (even-iter n acc)
    (if (= n x)
        acc
        (even-iter (inc n) (not acc))))
  (even-iter 0 true))
 

(define (simpsons-integral f a b n)
  (define (factor k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (let ((h (/ (- b a) n)))
    (define (y k) 
      (* (factor k) (f (+ a (* k h)))))
    (* (/ h 3) (sum y 0 inc n))))
    