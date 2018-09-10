;3.1
(define (make-accumulator a)
  (lambda (x)
    (begin (set! a (+ x a))
           a)))
(define A (make-accumulator 5))
(A 10)
(A 10)


;3.2
(define (make-monitored f)
  (let ((times 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) times)
            ((eq? x 'reset-count) (set! times 0) times)
            (else (set! times (+ times 1)) (f x))))))
(define s (make-monitored sqrt))

;3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (check amount)
    (eq? pass password))
  (define (dispatch pass m)
    (cond ((eq? m 'check) check)
          ((not (eq? pass password)) (error "wrong password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else  (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)

;3.7
(define (make-joint acc accpass mypass)
  (define (dispatch pass m)
    (cond ((not (eq? pass mypass)) (error "wrong password"))
          (else (acc accpass m))))
  (if (acc accpass 'check) dispatch (error "lol no")))
;in-class:
(define (make-joint oldacc oldpass newpass)
  (oldacc 'deposit oldpass)
  (lambda (m pass)
    (cond ((not (eq? pass newpass)) (error "Wrong password"))
          (else (oldacc m oldpass)))))

(define peter (make-account 100 'open-sesame))
;(define paul (make-joint peter 'open-sesame 'rosebud))
;((paul 'rosebud 'withdraw) 30)

;3.8
(define (b a)
  (lambda (x)
    (begin (set! a (* x a))
           a)))
(define f (b 1))
;in-class:
#|(define f
  (let ((state-variable -1))
    (lambda (n)
      (cond ((= n 0) (set! state-variable (+ state-variable 1)) state-variable)
            (else state-variable)))))|#

(+ (f 1) (f 0))
(+ (f 0) (f 1))


  