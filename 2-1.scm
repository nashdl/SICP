#lang racket

(require "shared.scm")

(define (make-rat n d) 
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (tests)
  (assert= '(1 . 5) (make-rat -1 -5))
  (assert= '(-1 . 5) (make-rat -1 5))
  (assert= '(-1 . 5) (make-rat 1 -5))
  (assert= '(1 . 5) (make-rat 1 5))
  )