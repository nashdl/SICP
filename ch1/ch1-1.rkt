#lang racket
(define (ex-1.2)
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7))))


;;  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define (ex-1.3 x y z)
  (cond ((< x y z) (sum-of-squares y z))
        ((< y x z) (sum-of-squares x z))
        (else (sum-of-squares x y))))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.7 . An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess.

(define (good-enough2? old-guess guess)
  (< (abs (- guess old-guess)) 0.0000000000001))

(define (sqrt-iter2 old-guess guess x)
  (if (good-enough2? old-guess guess)
      guess
      (sqrt-iter2 guess (improve guess x) x)))

(define (sqrt2 x)
  (sqrt-iter2 0.0 1.0 x))


;; Exercise 1.8.  Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value
(define (cube x)
  (* x x x))

(define (improve-cubert guess radicand)
  (/ (+ (/ radicand (square guess)) (* 2 guess))
     3))

(define (cubert-good-enough? guess radicand)
  (< (abs (- (cube guess) radicand)) 0.001))

(define (cubert-iter guess x)
  (if (cubert-good-enough? guess x)
      guess
      (cubert-iter (improve-cubert guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))
