
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
