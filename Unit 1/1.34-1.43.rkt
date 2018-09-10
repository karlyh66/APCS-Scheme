;1.34
(define (f g) (g 2))
; (f f) calls (f 2), which calls (2 2), which is not a function

;1.37a
(define (cont-frac n d k)
  (define (help result k)
    (cond ((= k 0) (/ 1 result))
          (else (help (+ (/ (n k) result) (d k)) (- k 1)))))
  (help (d k) k))
(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 10)

;1.38
(define (e terms)
  (define (d1 term)
    (if (= (modulo term 3) 2) (* (/ (+ 1 term) 3) 2) 1))
  (+ (cont-frac (lambda (x) 1.0) d1 terms) 2))
(e 30)

;1.41
(define (double function)
  (lambda (x) (function (function x))))
(define (inc x)
  (+ 1 x))
(((double (double double)) inc) 5) ;= 21

;1.42
(define (compose func1 func2)
  (lambda (x) (func1 (func2 x))))
(define (square x)
  (* x x))
((compose square inc) 6)

;1.43
(define (repeated function times)
  (lambda (x) 
    (cond ((= times 1) (function x))
          (else ((compose function (repeated function (- times 1))) x)))))
((repeated square 2) 5)

;17.1
; Rod
; Chris
; (Chris Colin Hugh Paul)
; haha fail
; ((Rod Argent) Chris White)
; (Rod Argent Chris White)
; ((Rod Argent) (Chris White))
; Chris
; Colin Blunstone
; nothing

;17.2
(define (f1 x y)
  (list (append (cdr x) (list (car y)))))
(f1 '(a b c) '(d e f))

(define (f2 x y)
  (cons (cdr x) (list (cadr y))))
(f2 '(a b c) '(d e f))

(define (f3 x y)
  (append x x))
(f3 '(a b c) '(d e f))

(define (f4 x y)
  (list (list (car x) (car y)) (append (cdr x) (cdr y))))
(f4 '(a b c) '(d e f))

;17.3
;(map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4))
;list of procedures

;17.8
(define (member x list1)
  (cond ((null? list1) list1)
        ((equal? x (car list1)) list1)
        (else (member x (cdr list1)))))
(member 'd '(a b c d e f g))
(member 'd '())

;17.9
(define (list-ref list1 n)
  (if (= n 0) (car list1)
  (list-ref (cdr list1) (- n 1))))
(list-ref '(happiness is a warm gun) 3)

;17.10
(define (length list1)
  (define (haha list1 n)
    (if (null? list1) n (haha (cdr list1) (+ n 1))))
  (haha list1 0))
(length '(57 56 7765 4 53))

;17.11
(define (before-in-list? list1 a b)
  (cond ((null? (member a list1)) #f)
        ((null? (member b list1)) #f)
        ((null? (member b (member a list1))) #f)
        (else #t)))
(before-in-list? '(back in the ussr) 'in 'ussr)
(before-in-list? '(back in the ussr) 'the 'back)
(before-in-list? '(back in the ussr) 'the 'bak)

;17.12
(define (flatten list1)
  (define list2 '())
  (define (flatten-helper list1)
    (if (null? list1) list2
    (if (list? (car list1)) (and (flatten-helper (car list1)) (flatten-helper (cdr list1)))
        (and (set! list2 (se list2 (car list1))) (flatten-helper (cdr list1))))))
  (flatten-helper list1))

(flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))

;17.14
(define (branch list1 list2)
  (cond ((null? list1) list2)
        (else (branch (cdr list1) (list-ref list2 (- (car list1) 1))))))
(branch '(2 3 1 2) '((a b) ((c d) (e f) ((g h) (i j)) k) (l m)))

(list? '(a))




  