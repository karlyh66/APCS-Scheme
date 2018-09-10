;Exercise 1.8
(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.0001))

(define (cuberoot x)
  (cuberoothelper 1 x))

(define (cuberoothelper guess x)
  (if (good-enough? guess x)
      guess
      (cuberoothelper (improve guess x) x)))

(define (improve guess x)
  (exact->inexact (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)))

#|
Exercise 1.9
a) recursive
(+ 4 5)
inc (+ 3 5)
inc inc (+ 2 5)
inc inc inc (+ 1 5)
inc inc inc inc (+ 0 5)
inc inc inc inc 5
inc inc inc 6
inc inc 7
inc 8
9

b) iterative
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
|#

#|
Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                (A x (- y 1))))))
(A 1 10) = 1024
(A 2 4) = 65536
(A 3 3) = 65536
(f n) = 2n
(g n) = 2^n
(h n) = 2^2^2^2^2... will be n 2's
(k n) = 5n^2
|#

;Exercise 1.16
(define (expt b n)
  (expthelp 1 b n))

(define (expthelp a b n)
  (cond ((= n 0) a)
        ((even? n) (expthelp a (* b b) (/ n 2)))
        (else (expthelp (* a b) b (- n 1)))))

(define (even? n)
  (= (remainder n 2) 0))