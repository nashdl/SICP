; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (boolean=? (positive? n) (positive? d))
                  1
                  (- 1))))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) ( denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) ( denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-third (make-rat 1 3))

(define neg-one-third (make-rat (- 1) 3))

(print-rat (add-rat one-third neg-one-third))
