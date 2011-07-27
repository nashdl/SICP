(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (make-center-percent c p)
  (make-interval
    (* c (/ (- 100.0 p) 100.0))
    (* c (/ (+ 100.0 p) 100.0))))

(define (make-interval a b) (cons a b))

(define (print-interval i)
  (display (lower-bound i))
  (display "..")
  (display (upper-bound i))
  (newline))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (div-interval x y)
  (if (= (upper-bound y) (lower-bound y))
      (error "Division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (print-center-percent i)
  (display (center i))
  (display " +/- ")
  (display (percent i))
  (display "%")
  (newline))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* 100.0 (/ (- (upper-bound i) (center i))
              (center i))))

(define (compare c1 p1 c2 p2)
  (define a (make-center-percent c1 p1))
  (define b (make-center-percent c2 p2))

  (display "a = ")
  (print-center-percent a)
  (display "b = ")
  (print-center-percent b)
  (newline)
  (display "Using formula 1, a/a = ")
  (print-center-percent (par1 a a))
  (display "Using formula 2, a/a = ")
  (print-center-percent (par2 a a))
  (newline)
  (display "Using formula 1, a/b = ")
  (print-center-percent (par1 a b))
  (display "Using formula 2, a/b = ")
  (print-center-percent (par2 a b))
  (newline))

(compare 10 2 12 3)
(compare 5 1 8 5)
(compare 2468 21 15 18)

; par2 consistently yields a lower percentage.
