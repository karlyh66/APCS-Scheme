#|
Dylan Fife
Karly Hou
Catherine Lee

Given a queue of functions, a value, and a target value, write the function bang! (pronounced bang bang) that applies the functions
in the queue to the value until it exceeds the target value. Else, return "lol no" or #f.
|#

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (make-queue) (cons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (insert! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair))))) 



(define (bang! queue value target)
  (cond ((> value target) value)
        ((empty-queue? queue) (display '(lol no)) (newLine))
        (else
         (let ((function (front-ptr queue)))          
           (set-front-ptr! queue (cdar queue))
           (bang! queue ((car function) value) target)
           ))))

(define (square x)
  (* x x))
(define (plus x)
  (+ x 1))
(define z (make-queue))
(insert! z square)
(insert! z plus)
(insert! z square)

(bang! z 2 5)
                              