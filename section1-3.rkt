
#lang racket

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-recur term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)))) 

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (cube x)
  (* x x x))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x ( + x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons-integral f a b n)
  (define (factor k)
    (if (< 0 k n)
        (if (even? k) 2 4)
        1))
  (let ((h (/ (- b a) n)))
    (define (y k) 
      (* (factor k) (f (+ a (* k h)))))
    (* (/ h 3) (sum y 0 inc n))))

(define (factorial x)
  (product identity 1 inc x))

(define (pi-over-four b)
  (define (term pred)
    (lambda (n) (if (pred n) (+ n 1) (+ n 2))))
  (let ((add-one-first (term odd?)) (add-two-first (term even?)))
    (define (series term)   
      (product term 1 inc b))
    (/ (series add-one-first) (series add-two-first))))