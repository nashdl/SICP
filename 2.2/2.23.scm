(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

(define (for-each f l)
  (if (null? l)
      true
      ((f (car l))
       (for-each f (cdr l)))))
