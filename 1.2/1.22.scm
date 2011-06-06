(load "1.21.scm")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      ""))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime) (current-milliseconds))
;timer doesn't seem to work

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes start count)
  (cond ((= count 0) )
        ((even? start) (search-for-primes (+ start 1) count))
        (else (
          (timed-prime-test start)
          (search-for-primes (+ start 2) (if (prime? start) (- count 1) count))))))

;(search-for-primes 1000 3) => 1009, 1013, 1019
;(search-for-primes 10000 3) => 10007, 10009, 10037
;(search-for-primes 100000 3) => 100003, 100019, 100043
;(search-for-primes 1000000 3) => 1000003, 1000033, 1000037
