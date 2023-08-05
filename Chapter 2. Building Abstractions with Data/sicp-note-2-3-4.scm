 ;; ----------------------------------------------- 
 ;; EXERCISE 2.68 
 ;; ----------------------------------------------- 
  
 ;; helpers 
 (define true #t)
 (define false #f)

 (define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) true) 
         (else (element-of-set? x (cdr set))))) 
 (define (make-leaf symbol weight) 
   (list 'leaf symbol weight)) 
 (define (leaf? object) 
   (eq? (car object) 'leaf)) 
 (define (symbol-leaf x) (cadr x)) 
 (define (left-branch tree) (car tree)) 
 (define (right-branch tree) (cadr tree)) 
;  在定义时，对非leaf，会在符号部分存其所有子节点中包含的符号，是一个表，见make-code-tree
 (define (symbols tree) 
   (if (leaf? tree) 
       (list (symbol-leaf tree)) 
       (caddr tree))) 
 (define (weight-leaf x) (caddr x)) 
 (define (make-code-tree left right) 
   (list left 
         right 
         (append (symbols left) (symbols right)) 
         (+ (weight left) (weight right)))) 
 (define (weight tree) 
   (if (leaf? tree) 
       (weight-leaf tree) 
       (cadddr tree))) 
  
 (define (encode message tree) 
   (if (null? message) 
       '() 
       (append (encode-symbol (car message) tree) 
               (encode (cdr message) tree)))) 
  
 ;; solution 
 (define (encode-symbol smb tree) 
   (define (branch-correct? branch) 
     (if (leaf? branch) 
         (equal? smb (symbol-leaf branch)) 
         (element-of-set? smb (symbols branch)))) 
  
   (let ((lb (left-branch tree)) 
         (rb (right-branch tree))) 
     (cond ((branch-correct? lb) 
            (if (leaf? lb) '(0) (cons 0 (encode-symbol smb lb)))) 
           ((branch-correct? rb) 
            (if (leaf? rb) '(1) (cons 1 (encode-symbol smb rb)))) 
           (else (error "bad symbol -- ENCODE-SYMBOL" bit))))) 
  
 ;; tests 
 (define sample-tree 
   (make-code-tree (make-leaf 'A 4) 
                   (make-code-tree 
                    (make-leaf 'B 2) 
                    (make-code-tree (make-leaf 'D 1) 
                                    (make-leaf 'C 1))))) 
 (encode '(A D A B B C A) sample-tree) 
 ; (0 1 1 0 0 1 0 1 0 1 1 1 0) 
; 判断指定符号是否在给定表中
(define (memq item lst)
  (cond ((null? lst) #f)                 ; If the list is empty, return #f (not found).
        ((eq? item (car lst)) lst)      ; If the item matches the first element of the list, return the list.
        (else (memq item (cdr lst)))))  ; Otherwise, recursively search in the rest of the list.


  (define (encode message tree) 
   (if (null? message) 
       '() 
       (append (encode-symbol (car message) tree) 
               (encode (cdr message) tree)))) 
  
 (define (encode-symbol sym tree) 
   (if (leaf? tree) 
       (if (eq? sym (symbol-leaf tree)) 
           '() 
           (error "missing symbol: ENCODE-SYMBOL" sym)) 
       (let ((left (left-branch tree))) 
         (if (memq sym (symbols left)) 
             (cons 0 (encode-symbol sym left)) 
             (cons 1 (encode-symbol sym (right-branch tree))))))) 