(define (make-bintree datum left right)
  (lambda (msg)
    (cond ((= msg 0) datum)
          ((= msg 1) left)
          ((= msg 2) right))))

(define (datum bintree) (bintree 0))
(define (left bintree) (bintree 1))
(define (right bintree) (bintree 2))

(define tet 'toasterhead)
(define (empty-tree? tree) 
  (eq? tree 'toasterhead))
(define (leaf? bintree)
  (and (not (empty-tree? bintree))
       (empty-tree? (left bintree))
       (empty-tree? (right bintree))))


(define bst
  (make-bintree 15
     (make-bintree 6
        (make-bintree 2 tet tet)
        tet)
     (make-bintree 22
        (make-bintree 17
           (make-bintree 16 tet tet)
           (make-bintree 19 tet tet))
        (make-bintree 24 tet tet))))


;1
(define (contains? tree number)
  (cond ((empty-tree? tree) #f)
        ((= (datum tree) number) #t)
        (else (or (contains? (left tree) number)
                  (contains? (right tree) number)))))
(contains? bst 18)

;2
(define (inorder tree)
  (cond ((empty-tree? tree) '())
        (else (append (inorder (left tree)) (list (datum tree)) (inorder (right tree))))))
(inorder bst)

;3
(define (count-nodes tree)
  (cond ((empty-tree? tree) 0)
        (else (+ 1 (count-nodes (left tree)) (count-nodes (right tree))))))
(count-nodes bst)

;4
(define (square x)
  (* x x))
(define (square-tree tree)
  (cond ((empty-tree? tree) tet)
        (else (make-bintree (square (datum tree)) (square-tree (left tree)) (square-tree (right tree))))))
(datum (left (left (right (square-tree bst)))))

;2A
(define (maxpath tree)
  (cond ((empty-tree? tree) 0)
        (else (max (+ (datum tree) (maxpath (left tree)))
                   (+ (datum tree) (maxpath (right tree)))))))
(maxpath bst)

;2B
(define (pathway key tree)
  (cond ((empty-tree? tree) #f)
        ((= key (datum tree)) '(done))
        ((> key (datum tree)) (append '(right) (pathway key (right tree))))
        (else (append '(left) (pathway key (left tree))))))
        
(pathway 19 bst)

;3
(define (deepmap f l)
  (cond ((null? l) '())
        ((list? (car l)) (list (deepmap f (car l))))
        (else (append (list (f (car l))) (deepmap f (cdr l))))))
(deepmap square '(1 2 3 4 5 (5 6)))
(deepmap (lambda (x) (cons x 'l)) '(dillon tavor alex (pikachu peter (jeffrey))))
       
        

  