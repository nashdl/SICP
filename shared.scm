#lang racket

(provide assert=)

(define (assert= x y)
  (newline)
  (if (equal? x y)
      (displayln "Passed.")
      (displayln (format "Expected ~a to equal ~a." x y))))
