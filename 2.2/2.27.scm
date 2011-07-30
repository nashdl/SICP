(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

(define (deep-reverse l)
  (cond ((not (pair? l)) l)
        ((null? l) l)
        (else (append (deep-reverse (cdr l)) 
                      (list (deep-reverse (car l)))))))
  
(define x (list (list 1 2) (list 3 4)))

(display (reverse x))
(newline)
(display (deep-reverse x))
(newline)
