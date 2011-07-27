(define nil (list))

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(display (square-list (list 1 2 3 4)))
(newline)

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(display (square-list (list 1 2 3 4)))
(newline)
