;3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x '( a b c))

(set-car! (cdr x) (cddr x))

(count-pairs x)

(define z '(a b c))
(set-car! z (cdr z))
(set-car! (cdr z) (cddr z))

(count-pairs z)
(count-pairs (cdr z))

(define y '(a b c))

(set-car! (cdr y) y)

;(count-pairs y)


;3.17
(define (count-pairs x)
  (let ((visited '()))
    (define (ohno x l)
      (cond ((not (pair? l)) #f)
            ((eq? x (car l)) #t)
            (else (ohno x (cdr l)))))
    (define (yikes x)
      (cond ((not (pair? x)) 0)
            ((ohno x visited) 0)
            (else (begin (display visited) (newLine) (set! visited (cons x visited))
                         (+ (yikes (car x)) (yikes (cdr x)) 1)
                         ))))
    (yikes x)))



;3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (insert x)
      (let ((new-pair (cons x '())))
        (if (null? rear-ptr)
            (begin (set! rear-ptr new-pair)
                   (set! front-ptr new-pair))
            (begin (set-cdr! rear-ptr new-pair)
                   (set! rear-ptr new-pair)))))
    (define (delete)
      (if (null? front-ptr)
          (display 'empty)
          (let ((deleted (car front-ptr)))               
            (begin (set! front-ptr (cdr front-ptr))
                   (display deleted)))))
    (define (dispatch m)
      (cond ((eq? m 'insert) insert)
            ((eq? m 'delete) delete)
            (else (display 'lol))))              
    dispatch))

(define z (make-queue))
((z 'insert) 'a)
((z 'insert) 'b)
((z 'delete))


;delete problem

(define (make-table)
  (cons '* '()))

(define (empty-table? t) (null? (cdr t)))

(define (insert! key value table)
  (let ((record (assoc1 key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
    
(define (lookup k t)
  (let ((record (assoc1 k (cdr t))))
    (cond (record (cdr record))
          (else #f))))

(define (assoc1 key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc1 key (cdr records)))))

(define (rlookup k t)
  (let ((record (rassoc k (cdr t))))
    (cond (record (car record))
          (else #f))))

(define (rassoc value records)
  (cond ((null? records) #f)
        ((equal? value (cdar records)) (car records))
        (else (rassoc value (cdr records)))))

(define (delete! k t)
  (let ((record t))
    (define (help t)
      (cond ((null? t) #f)
            ((equal? (caar t) k) (set-cdr! record (cdr t)))
            (else (set! record t) (help (cdr t)))))
    (help (cdr t))))

;3.25
(define (make-table)
  (cons '* '()))

(define (insert! keylist value table)
  (define (help keylist2 table2)
    (let ((record (assoc1 (car keylist2) (cdr table2))))
      (cond ((not record) (set-cdr! table2 (cons (inside keylist2 value) (cdr table2))))
            ((null? (cdr keylist2)) (set-cdr! record value))
            (else (help (cdr keylist2) record)))))
  (define (inside keylist value)
    (cond ((null? keylist) value)
          (else (list (car keylist) (inside (cdr keylist) value)))))             
  (help keylist table))

(define (assoc1 key table)
  (cond ((null? table) #f)
        ((equal? (caar table) key) (car table))
        (else (assoc1 key (cdr table)))))

(define (lookup keylist table)
  (let ((record (assoc1 (car keylist) (cdr table))))
    (cond ((not record) #f)
          ((null? (cdr keylist)) (cdr record))
          (else (lookup (cdr keylist) record)))))
        
(define z (make-table))
(insert! '(1 2 3) 'a z)
(display z)
(newLine)
(insert! '(1 2 4) 'a z)
(display z)
(newLine)
(insert! '(1 5 4) 'a z)
(display z)
(newLine)
(lookup '(1 5) z)


