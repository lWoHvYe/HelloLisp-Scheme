(define (count-leaves x)
    (cond ((null? x) 0)
        ((not (pair? x)) 1) ; pair?用于检查是否为序对
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(display (count-leaves (cons (list 1 2 3) (list 4 5 6))))(newline)

(define (scale-there tree factor)
    (map (lambda (sub-tree) 
                 (if (pair? sub-tree)
                     (scale-tree sub-tree factor)
                     (* sub-tree factor)))
          tree)) 