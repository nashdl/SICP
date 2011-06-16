#lang racket

(provide assert=)

(define (assert= expected actual)
  (newline)
  (if (equal? expected actual)
      (displayln "Passed.")
      (displayln (format "Expected ~a to equal ~a." actual expected))))
