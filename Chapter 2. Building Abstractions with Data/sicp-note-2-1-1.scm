; 序对 cons 表示 construct构造，
; car表示 Constents of Address part of Register(寄存器的地址部分的内容)
; cdr(读作 could-er)表示 Constents of Decrement part of Register(寄存器的减量部分的内容)
; 从序对构造起来的数据对象称为表结构数据
(define  x (cons 1 2))

(display (car x)) (newline)

(display (cdr x))