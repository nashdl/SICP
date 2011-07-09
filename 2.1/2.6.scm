(define zero (lambda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (+ a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

(define (inc n) (- n (- 1)))

(display (((+ two one) inc) 0))

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f x)))
; (lambda (f) f)
; Take f and call it once.
