#lang racket

(require "test.scm")

(define (make-rat n d)   
  (define (same-sign?) (positive? (* n d)))
  (let ((g (gcd n d))
        (numer-op (if (same-sign?) 
                      abs 
                      (lambda (x) (- (abs x))))))    
    (cons (/ (numer-op n) g) 
          (/ (abs d) g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-pair p first second sep)
  (newline)
  (display (first p))
  (display sep)
  (display (second p)))

(define (print-rat x)
  (print-pair x numer denom "/"))

(define (test-2.1)
  (assert= '(1 . 5) (make-rat -1 -5))
  (assert= '(-1 . 5) (make-rat -1 5))
  (assert= '(-1 . 5) (make-rat 1 -5))
  (assert= '(1 . 5) (make-rat 1 5)))

(define (make-segment s e)
  (cons s e))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (print-pair p x-point y-point ","))

(define (midpoint-segment seg)
  (define (avg x y) (/ (+ x y) 2))
  (define (point-avg selector) 
    (avg (selector (start-segment seg)) 
         (selector (end-segment seg))))
  (make-point (point-avg x-point)
              (point-avg y-point)))

(define (test-2.2)
  (let ((seg (make-segment (make-point 1 3) (make-point 2 4))))
    (assert= (make-point 3/2 7/2) (midpoint-segment seg)))
  (let ((seg (make-segment (make-point 8 17) (make-point 4 21))))
    (assert= (make-point 6 19) (midpoint-segment seg))))




(define (all-tests)
  (test-2.1)
  (test-2.2))