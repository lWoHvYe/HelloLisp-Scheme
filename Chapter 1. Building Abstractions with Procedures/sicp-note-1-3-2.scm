(define (pi-sum a b)
    (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
    a
    (lambda (x) (+ x 4))
    b
))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))

(display (* 8 (pi-sum 1 1e4)))

(define (square x) 
  (* x x))

; 使用内部定义
(define (f x y)
    (define a (+ 1 (* x y)))
    (define b (- 1 y))
    (+ (* x (square a))
       (* y b)
       (* a b)))
(display (f 1 2)) (newline)

; 利用辅助过程约束局部变量
(define (f x y)
    (define (f-helper a b)
        (+ (* x (square a))
           (* y b)
           (* a b)))
    (f-helper (+ 1 (* x y)) (- 1 y)))
(display (f 1 2)) (newline)

; 使用lambda描述约束局部变量的匿名过程
(define (f x y)
    ((lambda (a b) ; 两个参数a b 
       (+ (* x (square a))
          (* y b)
          (* a b)))
          (+ 1 (* x y)) (- 1 y))) ;跟在lambda表达式后的调用参数
(display (f 1 2)) (newline)

; 使用let创建局部变量
(define (f x y)
    (let ((a (+ 1 (* x y)))
         (b (- 1 y)))
        (+ (* x (square a))
            (* y b)
            (* a b))))
(display (f 1 2)) (newline)