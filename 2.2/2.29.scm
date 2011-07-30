(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;b.
(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

;c.
(define (branch-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (or (not (pair? mobile))
      (and (= (branch-torque (left-branch mobile))
              (branch-torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

(define m (make-mobile
            (make-branch 2 16)
            (make-branch 4 
              (make-mobile
                (make-branch 3 5)
                (make-branch 5 3)))))

(display (balanced? m))
(newline)

;d.
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

(define m2 (make-mobile
            (make-branch 2 16)
            (make-branch 4 
              (make-mobile
                (make-branch 3 5)
                (make-branch 5 3)))))

(display (balanced? m2))
(newline)
