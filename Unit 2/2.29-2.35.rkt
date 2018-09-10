;2.29
(define make-mobile cons)
(define left car)
(define right cdr)

(define make-branch cons)
(define len car)
(define struct cdr)

(define weight? number?)
(define mobile? pair?)

(define (total-weight mobile)
  (define (help mobile2)
    (cond ((weight? mobile2) mobile2) 
          (else (+ (help (struct (left mobile2))) (help (struct (right mobile2)))))))
  (help mobile))
  
; Here is a test mobile so we can have a sample input to the
; programs, below.  Note that we must adhere to the abstractions.

(define m
  (make-mobile
   (make-branch 6
    (make-mobile
     (make-branch 1 8)
     (make-branch 4 2)))
   (make-branch 5 12)))

;2.30
(define (square x)
  (* x x))

(define (square-tree list1)
  (cond ((null? list1) '())
        ((list? (car list1)) (cons (square-tree (car list1)) (square-tree (cdr list1))))
        (else (cons (square (car list1)) (square-tree (cdr list1))))))

(define (map-square-tree list1)
  (map (lambda (list1)
         (if (list? list1) (map-square-tree list1) (square list1))) list1))

;2.31
(define (tree-map func list1)
  (map (lambda (list1)
         (if (list? list1) (tree-map func list1) (func list1))) list1))
;2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
(subsets (list 1 2 3))
;starts by creating all the subsets with the last element, then continues by creating the subsets
;the last two elements are in by adding the second to last element to all the previous subsets
;continues with the rest


;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1)) 
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (x) (if (list? x) (count-leaves x) 1)) t)))

(count-leaves (list 1 2 3 4 (list 5 6 7 (list 2 3))) )










