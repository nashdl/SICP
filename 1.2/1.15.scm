(define (cube x) (* x x x))
(define (p x) (display ".") (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;a. (sine 12.15) evaluates p 5 times.
;
;b. Order of growth in space and number of steps are equal in linear recursion, right?
;   The function adds another step every time the angle triples.
;   Does that mean the order of growth is theta(a)?
