#lang racket
;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n)) ;; 2n

(define (g n) (A 1 n)) ;; 2^n
#|
(A 1 0)
0
(A 1 1)
2
(A 1 2)
(A 0 (A 1 1))
(A 0 2)
4
(A 1 3)
(A 0 (A 1 2))
(A 0 (A 0 (A 0 1)))
(A 0 (A 0 2))
(A 0 4)
8
|#

(define (h n) (A 2 n))
#|
(A 2 0)
0

(A 2 1)
2

(A 2 2)
(A 1 (A 2 1))
(A 1 2)
(A 0 (A 1 1))
(A 0 2)
4

(A 2 3)
(A 1 (A 2 2))
(A 1 4)
(A 0 (A 1 3)) ;; 2(2^n)
(A 0 8)
16

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 4))
(A 1 16)
;; 2^(n^2)

(A 2 5)
(A 1 (A 2 4))
(A 1 (A 1 (A 2 3)))
(A 1 (A 1 (A 1 (A 2 2))))
(A 1 (A 1 (A 1 (A 1 (A 2 1)))))
(A 1 (A 1 (A 1 (A 1 2))))
|#

(define (k n) (* 5 n n))


#|
Exercise 1.11.  A function f is defined by the rule that
f(n) = n if n<3 and
f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3.
Write a procedure that computes f by means of a recursive process.
Write a procedure that computes f by means of an iterative process.
|#

(define (recursive-f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))


