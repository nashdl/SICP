#lang racket

(require "test.scm")

(define (make-rat n d) 
  ;(define (both pred pair) (and (pred (car pair)) (pred (cdr pair))))
  (let ((g (gcd n d))
        (same-sign? (or (and (negative? n) (negative? d)) (and (positive? d) (positive? n)))))    
    (cons (/ (if same-sign? (abs n) (- (abs n))) g) 
          (abs (/ d g)))))

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