# Exercise 1.1
    10
    12
    8
    3
    6
    19
    #f (false)
    4
    16
    6
    12

# Exercise 1.2

    ( / (+ 5 4 
        (- 2 
          (- 3 
            (+ 6 (/ 4 5))
          )
        )
      )
      (* 3 
        (- 6 2)
        (- 2 7)
      )
    )

# Exercise 1.3
## Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.


    (define (sum_of_squares x y) (+ (* x x) (* y y)))
    (define (f x y z) 
      (cond 
        ((and (< x y) (< x z)) (sum_of_squares y z))
        ((and (< y z) (< y x)) (sum_of_squares x z))
        ((and (< z x) (< z y)) (sum_of_squares x y))
      )
    )

# Exercise 1.4

In this procedure, if b is greater than 0 then the result is the sum of a and b. Otherwise, the result is the difference between a and b.

# Exercise 1.5

Not sure I completely understand this one. p sends scheme into an infinite loop, so it appears to have evaluated it even though it was not necessary.

# Exercise 1.6

My guess here is that the interpreter is attempting to continue expanding out 'new-if', so it catches itself inside an infinite loop.

# Exercise 1.7

    (define (sqrt-iter old-guess new-guess x)
      (if (good-enough? old-guess new-guess)
            new-guess
                  (sqrt-iter new-guess (improve new-guess x)
                                   x)))
    (define (improve guess x)
      (average guess (/ x guess)))
    (define (average x y)
      (/ (+ x y) 2))
    (define (good-enough? old-guess guess)
      (< (abs (- guess old-guess)) 0.000001))
    (define (mysqrt x)
      (sqrt-iter 0.1 1.0 x))
    (mysqrt (* 5209350235.1 5209350235.1))

# Exercise 1.8

    (define (cube-iter old-guess new-guess x)
      (if (good-enough? old-guess new-guess)
            new-guess
                  (cube-iter new-guess (improve new-guess x)
                                   x)))
    (define (improve y x)
      (/ (+ (/ x (* y y)) (* 2 y)) 3)
      )
    (define (good-enough? old-guess guess)
      (< (abs (- guess old-guess)) 0.000001))
    (define (cube-root x)
      (cube-iter 0.1 1.0 x))
    (cube-root (* 5 5 5))

# Exercise 1.9

    (define (inc x) (+ x 1))
    (define (dec x) (- x 1))
    (inc 1)
    (define (my-plus a b)
      (if (= a 0)
            b
            (inc (my-plus (dec a) b))))
    (my-plus 4 5)

    (my-plus 4 5)
    (my-plus (inc (dec 4) 5)
    (my-plus (inc (dec 4) 5) # 3 6
    (my-plus (inc (inc (dec (dec 4)) 5)) # 2 7
    (my-plus (inc (inc (inc (dec (dec (dec 4))) 5))) # 1 8
    (my-plus (inc (inc (inc (inc (dec )(dec (dec (dec 4))) 5)))) # 0 9

    (define (my-faster-plus a b)
      (if (= a 0)
            b
                  (my-faster-plus (dec a) (inc b))))
    (my-faster-plus 4 5)

    (my-faster-plus 4 5)
    (my-faster-plus 3 6)
    (my-faster-plus 2 8)
    etc...

It would appear to me that the second example is more likely to be tail-recursive. But I'm not 100% sure of that.

# Exercise 1.10
	first reaction: Looks to me like all of these will output 0?
	y would continue to be decremented until it was equal to 0 (last line of the method)
	- nope. wrong.

	(define (f n) (A 0 n))
	= 2n
	(define (g n) (A 1 n))
	= n^2
	(define (h n) (A 2 n))
	= 2^n^n

# Exercise 1.11
	Recursive:

	(define (myfunc n)
  	(cond ((< n 3) n)
	        (else (+ (myfunc (- n 1)) (* 2 (myfunc(- n 2)) (* 3 (myfunc(- n 3))))))
					  )
	)
