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

; Given a table, a value, and a new key, change the key of the value to the new key

(define (change-key table value new-key)
  (let ((record (rassoc value (cdr table))))
    (if record
        (set-car! record new-key)
        'oopsie)))

(define z (make-table))
(insert! 1 'a z)
(insert! 2 'b z)
(display z) (newLine)
(change-key z 'a 3)
(display z) (newLine)



