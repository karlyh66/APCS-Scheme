#| EXERCISE 1.1
10   12   8   3   6   19
#f   4    16  6   16
|#

#| EXERCISE 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
|#

#| EXERCISE 1.3
(define (sum_larger x y z)
    (cond ((and (> x z) (> y z)) (+ (* x x) (* y y)))
          ((and (> x y) (> z y)) (+ (* x x) (* z z)))
          ((and (> z x) (> y x)) (+ (* y y) (* z z)))))
|#

#| EXERCISE 1.4
If b > 0, we add a and b
Otherwise, we subtract b from a
This effectively adds a + sqrt(b)
|#

#| EXERCISE 1.5
Applicative-order evalulation: evaulates (p) continuously and never gets farther
Normal-order evaluation: 0
|#

#| EXERCISE 1.6
The program runs out of memory bc
In (new-if (good-enough? guess x) guess (sqrt-iter (improve guess x) x))),
the evaluator follows applicative-order and thus when it tries to evaluate the
else-clause, it has to call sqrt-iter again, which makes it evaluate the else-clause, etc.
Runs infinitely
|#

