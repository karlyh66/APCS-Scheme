;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
    (iter a 0))


(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (cube x) (* x x x))

;1.31 a
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
    (iter a 1))

(define (factorial a)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) a))


(exact->inexact (/ (product (lambda (x) (cond ((odd? x) (+ x 1)) (else(+ x 2)))) 1 (lambda (x) (+ x 1)) 5000)
(product (lambda (x) (cond ((odd? x) (+ x 2)) (else(+ x 1)))) 1 (lambda (x) (+ x 1)) 5000)))

;1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (add-acc term a next b)
  (accumulate + 0 term a next b))
(add-acc (lambda (x) x) 1 (lambda (x) (+ x 1)) 5)

(define (product-acc term a next b)
  (accumulate * 1 term a next b))
(product-acc (lambda (x) x) 1 (lambda (x) (+ x 1)) 5)

;1.33
(define (filtered-accumulate  filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)       
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))

(define (sum-odd-square a b)
  (filtered-accumulate odd? + 0 (lambda (x) (* x x)) a (lambda (x) (+ x 1)) b))

(sum-odd-square 1 5)

(define (gcd a x)
  (if (= x 0)
      a
      (gcd x (remainder a x))))

(define (product-relprime x)
  (define (relprime? a)
    (if (= (gcd a x) 1) #t #f))
  (filtered-accumulate relprime? * 1 (lambda(x) x) 1 (lambda (x) (+ x 1)) x))

(product-relprime 10)