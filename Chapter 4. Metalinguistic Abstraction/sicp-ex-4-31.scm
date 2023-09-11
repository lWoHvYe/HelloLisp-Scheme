;  这个还是很复杂的，基本没懂
 (define (apply procedure arguments env) 
     (cond ((primitive-procedure? procedure) 
            (apply-primitive-procedure  
             procedure 
             (list-of-arg-values arguments env))) 
           ((compound-procedure? procedure) 
            (eval-sequence 
              (procedure-body procedure) 
              (extend-environment 
                 (procedure-parameter-names procedure) 
                 (compound-procedure-args procedure arguments env) 
                 (procedure-environment procedure)))) 
           (else 
             (error "Unknown procedure type -- APPLY" procedure)))) 
  
 (define (lazy-param? param) (eq? 'lazy (cadr param))) 
 (define (lazy-memo-param? param) (eq? 'lazy-memo (cadr param))) 
 (define (eager-param? param) (symbol? param)) 
  
 (define (compound-procedure-args procedure arguments caller-env) 
     (define (build-list params arg-exps) 
         (define (build param exp) 
             (cond ((eager-param? param) (actual-value exp caller-env)) 
                   ((lazy-param? param) (delay-it exp caller-env)) 
                   ((lazy-memo-param? param) (delay-it-memo exp caller-env)) 
                   (else (error "Invalid paramemeter specification -- COMPOUND-PROCEDURE-ARGS" param)))) 
         (map build params arg-exps)) 
     (build-list (procedure-parameters procedure) arguments)) 
  
 (define (actual-value exp env) 
     (force-it (eval exp env))) 
  
 (define (delay-it exp env) (list 'thunk exp env)) 
 (define (delay-it-memo exp env) (list 'thunk-memo exp env)) 
 (define (thunk? obj) (tagged-list? obj 'thunk)) 
 (define (thunk-memo? obj) (tagged-list? obj 'thunk-memo)) 
 (define (thunk-exp thunk) (cadr thunk)) 
 (define (thunk-env thunk) (caddr thunk)) 
 (define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk)) 
 (define (thunk-value evaluated-thunk) (cadr evaluated-thunk)) 
  
 (define (force-it obj) 
     (cond ((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj))) 
           ((thunk-memo? obj) 
            (let ((result (actual-value (thunk-exp obj) (thunk-env obj)))) 
             (set-car! obj 'evaluated-thunk) 
             (set-car! (cdr obj) result) 
             (set-cdr! (cdr obj) '()) 
             result)) 
           ((evaluated-thunk? obj) 
            (thunk-value obj)) 
           (else obj))) 
  
 (define (procedure-parameter-names p) 
     (map (lambda (x) (if (pair? x) (car x) x)) (procedure-parameters p))) 

; --------------
 ; start Exercise 4.31 
 (define (procedure-parameters-ex4.31 p) 
   (define (name parameter) 
     (if (pair? parameter) 
       (car parameter) 
       parameter)) 
   (define (parameter-names parameters) 
     (if (null? parameters) 
       '() 
       (cons (name (car parameters)) 
             (parameter-names (cdr parameters))))) 
   (parameter-names (cadr p))) 
 (define (procedure-raw-parameters p) (cadr p)) 
  
 (define (apply-ex4.31 procedure arguments env) 
   (cond [(primitive-procedure? procedure) 
          (apply-primitive-procedure 
            procedure 
            (list-of-arg-values arguments env))]                   ; changed 
         ((compound-procedure? procedure) 
          (eval-sequence 
            (procedure-body procedure) 
            (extend-environment 
              (procedure-parameters procedure) 
              (list-of-delayed-args (procedure-raw-parameters procedure) arguments env) 
              (procedure-environment procedure))))                 ; changed 
         (else (error "Unknow procedure type: APPLY" 
                      procedure)))) 
  
 (define (list-of-delayed-args-ex4.31 raw_parameters exps env) 
   (define (arg-value raw_parameter exp) 
     (if (pair? raw_parameter) 
       (cond ((eq? (cadr raw_parameter) 'lazy) 
              (delay-it-no-memo exp env)) 
             ((eq? (cadr raw_parameter) 'lazy-memo) 
              (delay-it exp env)) 
             (else (error "Unknow parameter type LIST-OF-DELAYED-ARGS:" (cadr raw_parameter)))) 
       (actual-value exp env))) 
   (if (no-operands? exps) 
     '() 
     (cons (arg-value (car raw_parameters) 
                      (first-operand exps)) 
           (list-of-delayed-args-ex4.31 (cdr raw_parameters) 
                                 (rest-operands exps) 
                                 env)))) 
  
 (define (delay-it-no-memo exp env) 
   (list 'thunk-no-memo exp env)) 
 (define (thunk-no-memo? obj) 
   (tagged-list? obj 'thunk-no-memo)) 
  
 (define (force-it-ex4.31 obj) 
   (cond ((thunk? obj) 
          (let ((result (actual-value (thunk-exp obj) 
                                      (thunk-env obj)))) 
            (set-car! obj 'evaluated-thunk) 
            (set-car! (cdr obj) 
                      result)       ; replace exp with its value 
            (set-cdr! (cdr obj) 
                      '()) 
            result)) 
         ((evaluated-thunk? obj) (thunk-value obj)) 
         ((thunk-no-memo? obj) (actual-value (thunk-exp obj) 
                                             (thunk-env obj))) 
         (else obj)))       ; forget unneeded env 
  
  
 (define apply apply-ex4.31) 
 (define force-it force-it-ex4.31) 
 (define procedure-parameters procedure-parameters-ex4.31) 
 (define list-of-delayed-args list-of-delayed-args-ex4.31) 