; Exercise 2.2

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (define (average a b)
        (/ (+ a b) 2.0))
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(define point-a (make-point 0 0))
;(define point-b (make-point 3 5))
;(define segment-a-b (make-segment point-a point-b))
;(define midpoint (midpoint-segment segment-a-b))
;(print-point midpoint)

; Exercise 2.3

(define (make-rect corner-a corner-b)
  (cons corner-a corner-b))

(define (corner-a-rect r)
  (car r))

(define (corner-b-rect r)
  (cdr r))

(define (perimeter-rect r height-rec width-rec)
  (* 2 (+ (height-rec r) (width-rec r))))

(define (area-rect r height-rec width-rec)
  (* (height-rec r) (width-rec r)))

(define (height-rec r)
  (abs (- (y-point (corner-a-rect r))
          (y-point (corner-b-rect r)))))

(define (width-rec r)
  (abs (- (x-point (corner-a-rect r))
          (x-point (corner-b-rect r)))))


(define r (make-rect (make-point 0 0)
                     (make-point 4 6)))
(display (perimeter-rect r height-rec width-rec))
(newline)
(display (area-rect r height-rec width-rec))


; Exercise 2.3
(define (make-rect-2 height width)
  (cons height width))

(define (height-rec-2 r)
  (car r))

(define (width-rec-2 r)
  (cdr r))


(define r (make-rect-2 6 4))
(newline)
(display (perimeter-rect r height-rec-2 width-rec-2))
(newline)
(display (area-rect r height-rec-2 width-rec-2))
