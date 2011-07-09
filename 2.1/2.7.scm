(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (print-interval i)
  (display (lower-bound i))
  (display "..")
  (display (upper-bound i))
  (newline))

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

; Exercise 2.7
(define (lower-bound x) (car x))

; Exercise 2.8
; The difference of two intervals should be figure similarly to the sum but swapped.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9
; width(x) = (ux - lx) / 2
; width(x + y) = ((ux + uy) - (lx + ly)) / 2
;              = (ux + uy - lx -ly) / 2
;              = (ux - lx) + (uy - ly) / 2
;              = (ux - lx) / 2 + (uy - ly) / 2
;              = width(x) + width(y)

(let ((a (make-interval 1 2))
      (b (make-interval 5 6)))
  (display (width a))
  (newline)
  (display (width b))
  (newline)
  (display (width (mul-interval a b))))
  (newline)

; Exercise 2.10
(print-interval (div-interval (make-interval 1 5)
                              (make-interval 6 6)))

(define (div-interval x y)
  (if (= (upper-bound y) (lower-bound y))
      (error "Division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; (print-interval (div-interval (make-interval 1 5)
;                              (make-interval 6 6)))

; Exercise 2.11
(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((negative? ux)
            (cond ((negative? uy)
                    (make-interval (* ux uy) (* lx ly)))
                  ((positive? ly)
                    (make-interval (* lx uy) (* ux ly)))
                  (else
                    (make-interval (* lx uy) (* lx ly)))))
          ((positive? lx)
            (cond ((negative? uy)
                    (make-interval (* ux ly) (* lx uy)))
                  ((positive? ly)
                    (make-interval (* lx ly) (* ux uy)))
                  (else
                    (make-interval (* ux ly) (* ux uy)))))
          (else
            (cond ((negative? uy)
                    (make-interval (* ux ly) (* lx ly)))
                  ((positive? ly)
                    (make-interval (* lx uy) (* ux uy)))
                  (else
                    (make-interval (min (* ux ly) (lx uy)) (max (* ux uy) (lx ly)))))))))

; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval
    (* c (/ (- 100.0 p) 100.0))
    (* c (/ (+ 100.0 p) 100.0))))

(define (percent i)
  (* 100.0 (/ (- (upper-bound i) (center i))
              (center i))))

(define i (make-center-percent 10 10))
(display (percent i))
(newline)

