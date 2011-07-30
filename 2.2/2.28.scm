(define x (list (list 1 2) (list 3 4)))

(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? (car x)))
          (cons (car x) (fringe (cdr x))))
        (else (append
                (fringe (car x))
                (fringe (cdr x))))))

(display (fringe x))
(newline)
(display (fringe (list x x)))
