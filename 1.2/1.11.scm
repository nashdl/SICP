(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f2 n)
  (define (f-iter n max m3 m2 m1)
    (if (> n max)
        m1
        (f-iter (+ n 1)
                max
                m2
                m1
                (if (< n 3)
                    n
                    (+ m1 (* 2 m2) (* 3 m3))))))

  (f-iter 1 n 0 0 0)
)
