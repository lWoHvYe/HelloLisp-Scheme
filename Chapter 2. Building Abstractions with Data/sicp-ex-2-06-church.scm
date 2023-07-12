; 当我们在 SICP 中定义过程 `zero` 和 `add-one` 时，我们使用了一种称为 Church 数字的表示方法，这是一种将数字表示为函数的方法。
; 1. `zero` 过程：此zero非彼zero
;    - `zero` 过程接受一个参数 `f`，表示后继操作（这里定义/接收，可以后继使用，若只有zero，则未使用）。
;    - `zero` 过程返回一个新的过程，这个新的过程接受一个参数 `x` 并返回 `x` 本身。
;    以数学上的角度来看，`zero` 过程表示数字 0。它接受一个后继操作 `f`，但不对其应用，直接返回参数 `x`，相当于 0 加上任何数字都等于该数字本身。

; 2. `add-one` 过程：此加一亦非彼加一
;    - `add-one` 过程接受一个参数 `n`，表示一个数字（通过 `zero` 或其他经过 `add-one` 处理的数字）。
;    - `add-one` 过程返回一个新的过程，这个新的过程接受一个参数 `f`，表示后继操作（即加一操作）。
;    - 返回的新过程将参数 `n` 应用到 `f` 上，并将结果应用到参数 `x` 上, 之后再将f应用到上一步的结果。
;    以数学上的角度来看，`add-one` 过程表示数字加一的操作。它接受一个数字(过程) `n`，并返回一个新的数字，这个新数字比 `n` 大 `1`（给定的过程f多执行了一次，在嵌套的情况下含义更明显，每嵌套一层，f都较之前多执行一次）。
; 使用这种方式，我们可以通过重复应用 `add-one` 过程来表示任意的正整数。例如，通过 `add-one` 过程三次应用到 `zero` 上，我们可以表示数字 3。通过重复应用 `add-one`，我们可以构建出更大的数字。
; 这种表示方式充分利用了函数的特性，通过嵌套和组合来表示数字和运算。它在函数式编程中被广泛使用，并提供了一种简洁而强大的表示数字的方式。
; 个人认为，上面有一点不严谨，这里的加一，并不是我们理解的加一，更像是执行了一次给定的函数

(define zero (lambda (f) (lambda (x) x)))

(define add-one (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))

(define one (add-one zero))
(define two (add-one one))
(define three (add-one two))

; 测试输出
; (zero (lambda (x) (+ x 1))) 返回一个过程(以x为参数 (lambda (x) (+ x 1)),f 是(lambda (x) (+ x 1))，注参数x与f中的x是不同的概念) ，在这里f只被定义，未使用
(display ((zero (lambda (x) (+ x 1))) 2))
(newline)
(display ((three (lambda (x) (+ x 1))) 0)) ; 输出 3，这里的lambda表达式，如果给定的过程f会更明显，比如square，就是多执行一次
(newline)
; (add-one zero) 返回一个过程(以f为参数 (lambda (f) (lambda (x) (f ((n f) x))))，n是zero)，然后将其应用到 (lambda (x) (+ x 1)) (就是f)，返回一个新的过程(以x为参数) 
(display (((add-one zero) (lambda (x) (+ x 1))) 0))

; 当执行 `(add-one zero)` 时，

; add-one 是 (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))，当传人 n = zero 时，返回一个过程 (lambda (f) (lambda (x) (f ((n f) x))))，
; 对返回的过程，传入定义的过程f时，返回一个过程 (lambda (x) (f ((n f) x)))，
; 对返回的过程，传入定义的参数x时，返回一个过程 (f ((n f) x))
; 这里的 n是 zero，f和x都是传入的参数
; 所以(n f) 为将过程n应用到过程f上， (lambda (f) (lambda (x) x))，结果定义了f，并返回过程 (lambda (x) x)
; 而((n f) x))，表示将返回的过程应用到x，上一步定义了f，这里只是返回x自身
; (f ((n f) x))则将f再次应用到结果上，即将过程f应用到x
; 感觉还是很多不明白，比如在zero中定义f到目的，应该是接收传入的f，虽然zero中没用到，但外围过程可能用到了，比如这里的add-one
; 下面这个就易于理解很多，还是看题解吧，GPT有时不怎么靠谱
;   => (lambda (f) (lambda (x) (f (((lambda (a) (lambda (b) (a b))) f) x))))
;   => (lambda (f) (lambda (x) (f ((lambda (b) (f b)) x))))
;   => (lambda (f) (lambda (x) (f (f x))))
;  (define one (lambda (f) (lambda (x) (f x)))) 
;  (define two (lambda (f) (lambda (x) (f (f x))))) 
;  (define three (lambda (f) (lambda (x) (f (f (f x)))))) 
