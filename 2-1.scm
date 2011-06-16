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

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (test-2.1)
  (assert= '(1 . 5) (make-rat -1 -5))
  (assert= '(-1 . 5) (make-rat -1 5))
  (assert= '(-1 . 5) (make-rat 1 -5))
  (assert= '(1 . 5) (make-rat 1 5)))