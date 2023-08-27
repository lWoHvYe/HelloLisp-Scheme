(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s)) (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each display-line s))

(define (display-line x)
    (newline)
    (display x))

(define (stream-car stream)
    (car stream))

(define (stream-cdr stream)
    (force (cdr stream)))

(define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred (stream-car stream)) 
           (cons-stream (stream-car stream) (stream-filter pred (stream-cdr stream))))
          (else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object)
    (delayed-object))

(define delay (lambda (proc) (memo-proc (lambda () proc)))) 

(define (memo-proc proc)
    (let ((already-run? #f) (result #f))
        (lambda () 
           (if (not already-run?)
            (begin (set! result (proc)) (set! already-run? #t) result)
            result))))

(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
    (cons-stream
      (stream-car stream)
      (sieve (stream-filter 
                (lambda (x) (not (divisible? x (stream-car stream))))
                (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))


(define fibs (cons-stream 0
                          (cons-stream 1
                                       (add-stream (stream-cdr fibs) fibs))))

 (define (partial-sums s) 
   (add-streams s (cons-stream 0 (partial-sums s)))) 

; 这个函数实现了对输入序列 s 进行积分操作
; ones 序列是一个全是 1 的序列，integers 序列是一个递增的整数序列。
(define (integrate-series s)
              (stream-map * (stream-map / ones integers) s))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

; 流这一章节，主要是对流本身的理解，流是一个变化的，对流的操作是依次对流中元素的操作，每次操作操作一个元素，对流car拿到的是首个元素，cdr是除首个元素外的部分，两个流的运算是对应的每一项的运算(1->1, 2->2...)
; 对流的理解一直不完全，下面这个更是懵了
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) 
                                        (mul-series (stream-cdr s1) s2))))
  
(define (div-series nums dems) 
   (mul-series nums 
               (invert-series dems))) 
  
 (define tangent-series (div-series sine-series cosine-series)) 

 (define (sqrt-stream x)
    (define guesses
      (cons-stream 1.0 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses))) guesses)
 (display-stream (sqrt-stream 2))

 (define (pi-summands n)
    (cons-stream (/ 1.0 n) (stream-map - (pi-summands (+ n 2)))))

(define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

(display-stream pi-stream)

; 欧拉、交错级数序列加速器
(define (euler-transform s)
    (let ((s0 (stream-ref s 0)) ;Sn-1
          (s1 (stream-ref s 1)) ;Sn
          (s2 (stream-ref s 2)));Sn+1
        (cons-stream (- s2 (/ (square (- s2 s1)) (+ s0 (* -2 s1) s2)))
                     (euler-transform (stream-cdr s)))))
; 构建流的流
(define (make-tableau transform s)
    (cons-stream s (make-tableau transform (transform s))))
; 搭建超级加速器
(define (accelerated-sequence transform s)
    (stream-map stream-car (make-tableau transform s)))

(define (stream-limit stream tolerance)
        (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
                (stream-ref stream 1)
                (stream-limit (stream-cdr stream) tolerance)))

(define (ln2-summands n)
        (cons-stream (/ 1.0 n) 
                     (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
        (partial-sums (ln2-summands 1)))


(define (integral integrand initial-value dt)
    (define int (cons-stream initial-value (add-stream (scale-stream integrand dt) int))) 
    int)

(define (integral delayed-integrand initial-value dt)
    (define int (cons-stream initial-value 
                             (let ((integrand (force delayed-integrand)))
                             (add-stream (scale-stream integrand dt) int))))
    int)

(define rand
    (let ((x random-init))
        (lambda () (set! x (rand-update x))
           x)))