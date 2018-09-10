(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 2.1
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (or (and (< d 0) (>= n 0)) (and (< d 0) (<= n 0))) (cons (* -1 (/ n g)) (* -1 (/ d g))) (cons (/ n g) (/ d g)))))

; 2.2
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
              (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(print-point (midpoint-segment (make-segment (make-point -1 4) (make-point 5 6))))

; 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
(cdr (cons 1 2))


; 2.7
(define (make-interval a b) (cons a b))
(define lower-bound car)
(define upper-bound cdr)

; 2.8
(define (sub-interval x y)
  (let ((a (- (lower-bound x) (upper-bound y))) 
        (b (- (upper-bound x) (lower-bound y))))
    (if (> b a) (make-interval a b) (make-interval b a))))

; 2.9
#|
interval from a to b, c to d
width: (b-a)/2, (d-c)/2

ADDING
interval a+c to b+d
width: ((b+d)-(a+c))/2 = ((b-a) + (d-c))/2 = widths of original intervals added

SUBTRACTING
interval a-d to b-c (assume in order of ascending)
width: ((b-c)-(a-d))/2 = ((b-a) + (d-c)/2 = widths of original intervals added

counter example for MULTIPLICATION
(0 4) (7 9)
widths: 2, 1
width of combined: 18

counter example for DIVISION
(0 4) (7 9)
widths: 2, 1
width of combined: .2857 ish

|#


