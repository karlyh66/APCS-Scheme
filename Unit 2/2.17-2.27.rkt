;2.17
(define (last-pair list1)
  (if (null? (cdr list1)) list1 (last-pair (cdr list1))))
(last-pair (list 23 72 149 34))

;2.18
(define (reverse list1)
  (define (reverse-help list1 list2)
    (if (null? list1) list2 (reverse-help (cdr list1) (cons (car list1) list2))))
  (reverse-help list1 '()))
(reverse (list 1 4 9 16 25))

;2.20
(define (same-parity x . y) 
  (define (help list1 list2)
    (cond ((null? list2) list1) 
          ((odd? x) (if (odd? (car list2)) (help (append list1 (list (car list2))) (cdr list2))
                                       (help list1 (cdr list2))))
          (else (if (even? (car list2)) (help (append list1 (list (car list2))) (cdr list2))
                                    (help list1 (cdr list2))))))
  (help (list x) y))
(same-parity 1 2 4 5 6)

;2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      '()
      (cons (square(car items)) (square-list(cdr items)))))
(define (square-list2 items)
  (map square items))
(square-list (list 1 2 3 4))

;2.22
;Every time you cons, you are adding to the front of the answer, producing a reverse list
;Because answer is also a list, you just create a pair with a list in the front

;2.23
(define (for-each f list1)
  (if (null? (cdr list1)) (f (car list1)) (and (f (car list1)) (for-each f (cdr list1)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;2.24
;(list 1 (list 2 (list 3 4))) yields '(1 (2 (3 4)))
; -->[1| ]-->[ |/]
;             |
;             v
;            [2| ]-->[ |/]
;                     |
;                     v
;                    [3| ]-->[4|/]
;
;      (1(2(3 4)))
;         /   \
;        1  (2 (3 4))
;             /  \
;            2  (3 4)
;                /  \
;               3    4

;2.25
(cdaddr'(1 3 (5 7) 9))
(caar'((7)))
(cadadr(cadadr(cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

;(append x y) --> '(1 2 3 4 5 6)
;(cons x y) --> '((1 2 3) 4 5 6)
;(list x y) --> '((1 2 3) (4 5 6))

;2.27
(define (deep-reverse list1)
  (define (reverse-help list1 list2)
    (cond ((null? list1) list2)
          ((list? (car list1)) (reverse-help (cdr list1) (cons (reverse-help (car list1) '()) list2)))
          (else (reverse-help (cdr list1) (cons (car list1) list2)))))
  (reverse-help list1 '()))

(define (deep-reverse l)
  (cond ((null? l) '())
        ((list? (car l)) (append (deep-reverse (cdr l))
                                 (list (deep-reverse (car l)))))))
(define x (list (list 1 2) (list 3 (list 5 6 7) )))
(reverse x)
(deep-reverse x)

;2.19 (ec)
(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

;The order of the list coin-values does not affect the answer produced by cc, because you get to the ends of
; all branches of the tree, regardless of which order you reach the ends of the branches. In the end, you sum
; the results of each branch, so order doesn't matter.
              
  