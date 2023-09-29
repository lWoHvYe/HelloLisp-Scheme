 ;; if the variable is unbounded, cons 'unbound with it;  
 ;; otherwise, cons 'bounded with it. then we can use 
 ;; car to judge it's bounded or not. 
 (define (lookup-variable-value var env) 
   (define (env-loop env) 
     (define (scan vars vals) 
       (cond ((null? vars) 
              (env-loop (enclosing-environment env))) 
             ((eq? var (car vars)) 
              (cons 'bounded (car vals)))              ;; **** 
             (else (scan (cdr vars) (cdr vals))))) 
     (if (eq? env the-empty-environment) 
             (cons 'unbounded '())                        ;; *** 
         (let ((frame (first-frame env))) 
           (scan (frame-variables frame) 
                 (frame-values frame))))) 
   (env-loop env)) 
 (define (bound-variable? var) 
  (and (pair? var) (eq? (car var) 'bounded))) 
 (define (extract-variable-value var) 
  (cdr var)) 
  
 ev-variable 
   (assign val (op lookup-variable-value) (reg exp) (reg env)) 
   (test (op bound-variable?) (reg val)) 
   (branch (label bound-variable)) 
   (assign val (const unbounded-variable-error)) 
   (goto (label signal-error))  
 bound-variable 
   (assign val (op extract-variable-value) (reg val)) 
   (goto (reg continue)) 

;; -------------------------- another solution -----------------
 ;; some supporting procedures or objects 
  
  
 ;; (gensym) procedure 
 ;; returns: a unique generated symbol 
 ;; libraries: (chezscheme) 
  
 (define *UNBOUNDED-ERROR* (gensym)) 
 (define (unbounded-error? err) (eq? err *UNBOUNDED-ERROR*)) 
  
 (define (lookup-variable-value var env) 
   (define (env-loop env) 
     (define (scan vars vals) 
       (cond ((null? vars) 
              (env-loop (enclosing-environment env))) 
             ((eq? var (car vars)) 
              (car vals)) 
             (else (scan (cdr vars) (cdr vals))))) 
     (if (eq? env the-empty-environment) 
         *UNBOUNDED-ERROR* 
         (let ((frame (first-frame env))) 
           (scan (frame-variables frame) 
                 (frame-values frame))))) 
   (env-loop env)) 
  
 (define *NOT-PAIR-ERROR* (gensym)) 
 (define (not-pair-error? err) (eq? err *NOT-PAIR-ERROR*)) 
  
 (define *INCORRECT-ARITY-ERROR* (gensym)) 
 (define (incorrect-arity-error? err) (eq? err *INCORRECT-ARITY-ERROR*)) 
  
 (define *NOT-NUMBER-ERROR* (gensym)) 
 (define (not-number-error? err) (eq? err *NOT-NUMBER-ERROR*)) 
  
 (define *DIVISION-ZERO-ERROR* (gensym)) 
 (define (division-zero-error? err) (eq? err *DIVISION-ZERO-ERROR*)) 
  
 ;; (procedure-arity-mask proc) procedure 
 ;; returns: an exact integer bitmask identifying the accepted argument counts of proc 
 ;; libraries: (chezscheme) 
  
 ;; The bitmask is represented as two’s complement number with the bit at each index n set 
 ;; if and only if proc accepts n arguments. 
 ;; The two’s complement encoding implies that if proc accepts n or more arguments, the 
 ;; encoding is a negative number, since all the bits from n and up are set. For example, if 
 ;; proc accepts any number of arguments, the two’s complement encoding of all bits set is -1. 
  
 ;; (logbit? index int) procedure 
 ;; returns: #t if the speciﬁed bit is set, otherwise #f 
 ;; libraries: (chezscheme) 
  
 (define (correct-arity? impl args) 
   (let ([arity-mask (procedure-arity-mask impl)]) 
     (logbit? (length args) arity-mask))) 
  
 (define THE-ARITHMETIC-OPERATIONS (list + - * = / > <)) 
 (define (arithmetic-operation? op) (memq op THE-ARITHMETIC-OPERATIONS)) 
 (define (apply-primitive-procedure proc args) 
   (let ([impl (primitive-implementation proc)]) 
     (cond [(not (correct-arity? impl args)) *INCORRECT-ARITY-ERROR*] 
           [(or (eq? impl car) (eq? impl cdr)) (apply-safe-car&cdr impl args)] 
           [(arithmetic-operation? impl) (apply-safe-arithmetic impl args)] 
           [else (apply-in-underlying-scheme impl args)]))) 
  
 (define (all-number-arguments? args) 
   (andmap number? args)) 
  
 (define (apply-safe-arithmetic impl args) 
   (cond [(not (all-number-arguments? args)) *NOT-NUMBER-ERROR*] 
         [(or (eq? impl /)) (apply-safe-division args)] 
         [else (apply-in-underlying-scheme impl args)])) 
  
 (define (apply-safe-car&cdr impl args) 
   (cond [(not (pair? (car args))) *NOT-PAIR-ERROR*] 
         [else (apply-in-underlying-scheme impl args)])) 
  
 (define (apply-safe-division args) 
   (if (zero? (car (last-pair args))) 
       *DIVISION-ZERO-ERROR* 
       (apply-in-underlying-scheme / args))) 
  
 (define (extend-environment vars vals base-env) 
   (if (= (length vars) (length vals)) 
       (cons (make-frame vars vals) base-env) 
       (if (not (= (length vars) (length vals))) 
           *INCORRECT-ARITY-ERROR*))) 
  
 ;;; some controller code fragments 
 unbounded-val 
   (assign val (const unbounded-value-error)) 
   (goto (label signal-error)) 
  
 incorrect-arity 
   (assign val (const incorrect-arity-error)) 
   (goto (label signal-error)) 
  
 division-zero 
   (assign val (const division-zero-error)) 
   (goto (label signal-error)) 
  
 not-pair 
   (assign val (const not-pair-error)) 
   (goto (label signal-error)) 
    
 not-number 
   (assign val (const not-number-error)) 
   (goto (label signal-error)) 
  
 ev-variable 
   (assign val (op lookup-variable-value) (reg exp) (reg env)) 
   (test (op unbounded-error?) (reg val)) 
   (branch (label unbounded-val)) 
   (goto (reg continue)) 
  
 primitive-apply 
   (assign val (op apply-primitive-procedure) 
               (reg proc) 
               (reg argl)) 
   (restore continue) 
    
   (test (op incorrect-arity-error?) (reg val)) 
   (branch (label incorrect-arity)) 
    
   (test (op division-zero-error?) (reg val)) 
   (branch (label division-zero)) 
    
   (test (op not-pair-error?) (reg val)) 
   (branch (label not-pair)) 
  
   (test (op not-number-error?) (reg val)) 
   (branch (label not-number)) 
   (goto (reg continue)) 
  
 compound-apply 
   (assign unev (op procedure-parameters) (reg proc)) 
   (assign env (op procedure-environment) (reg proc)) 
   (assign env (op extend-environment) 
               (reg unev) (reg argl) (reg env)) 
  
   (test (op incorrect-arity-error?) (reg env)) 
   (branch (label incorrect-arity)) 
    
   (assign unev (op procedure-body) (reg proc)) 
   (goto (label ev-sequence)) 
  
 ;;;;;;;;;;;;;;;; 
 ;;;;;;TEST;;;;;; 
 ;;;;;;;;;;;;;;;; 
 ;;; EC-Eval input: 
 (define (square x) (* x x)) 
  
 (total-pushes = 3 maximum-depth = 3) 
 ;;; EC-Eval value: 
 ok 
  
 ;;; EC-Eval input: 
 (square 2 2) 
 incorrect-arity-error 
  
 ;;; EC-Eval input: 
 (square 'x) 
 not-number-error 
  
 ;;; EC-Eval input: 
 (square a) 
 unbounded-value-error 
  
 ;;; EC-Eval input: 
 (+) 
  
 (total-pushes = 3 maximum-depth = 3) 
 ;;; EC-Eval value: 
 0 
  
 ;;; EC-Eval input: 
 (-) 
 incorrect-arity-error 
  
 ;;; EC-Eval input: 
 (car 2 2) 
 incorrect-arity-error 
  
 ;;; EC-Eval input: 
 (car (cons 1 2)) 
  
 (total-pushes = 13 maximum-depth = 8) 
 ;;; EC-Eval value: 
 1 
  
 ;;; EC-Eval input: 
 (car 1) 
 not-pair-error 
  
 ;;; EC-Eval input: 
 (/ 1 1 1 1 0) 
 division-zero-error 
  
 ;;; EC-Eval input: 
 (/ 1 0) 
 division-zero-error 
  
 ;;; EC-Eval input: 
 (/ 0) 
 division-zero-error 