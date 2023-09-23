 ;; add the code marked. 
 (define (make-new-machine) 
  (let ((pc (make-register 'pc)) 
            (flag (make-register 'flag)) 
            (stack (make-stack)) 
            (the-instruction-sequence '()) 
            (instruction-number 0) 
            (trace-on false))                                                 ;; *** 
   (define (print-instruction-number) 
    (display (list "current instruction number is: " instruction-number)) 
    (set! instruction-number 0) 
    (newline)) 
   (let ((the-ops 
                  (list (list 'initialize-stack 
                                   (lambda () (stack 'initialize))) 
                        (list 'print-stack-statistics  
                                       (lambda () (stack 'print-statistics))))) 
             (register-table       
           (list (list 'pc pc) (list 'flag flag)))) 
    (define (allocate-register name) 
         (if (assoc name register-table) 
             (error "Multiply defined register: " name) 
                 (set! register-table 
                       (cons (list name (make-register name)) 
                                 register-table))) 
         'register-allocated) 
    (define (lookup-register name) 
         (let ((val (assoc name register-table))) 
          (if val 
              (cadr val) 
                  (begin 
                   (allocate-register name) 
                   (lookup-register name))))) 
    (define (execute) 
         (let ((insts (get-contents pc))) 
          (if (null? insts) 
              'done 
                  (begin 
                   (if trace-on                                          ;; *** 
                       (begin                                              ;; *** 
                            (display (caar insts))                  ;; *** 
                        (newline)))                                      ;; *** 
                    ((instruction-execution-proc (car insts))) 
                    (set! instruction-number (+ instruction-number 1)) 
                    (execute))))) 
         (define (dispatch message) 
          (cond ((eq? message 'start) 
                         (set-contents! pc the-instruction-sequence) 
                         (execute)) 
                ((eq? message 'install-instruction-sequence) 
                         (lambda (seq) (set! the-instruction-sequence seq))) 
                    ((eq? message 'allocate-register) allocate-register) 
                    ((eq? message 'trace-on) (set! trace-on true))               ;; *** 
                    ((eq? message 'trace-off) (set! trace-on false))             ;; *** 
                    ((eq? message 'get-register) lookup-register) 
                    ((eq? message 'install-operations) 
                         (lambda (ops) (set! the-ops (append the-ops ops)))) 
                    ((eq? 'instruction-number) print-instruction-number) 
                    ((eq? message 'stack) stack) 
                    ((eq? message 'operations) the-ops) 
                    (else (error "Unkown request -- MACHINE" message)))) 
         dispatch))) 
 (define (trace-on-instruction machine)               ;; *** 
  (machine 'trace-on))  
 (define (trace-off-instruction machine)               ;; *** 
  (machine 'trace-off)) 

;   -----------------
 ;; add a field to instruction to include label. and change the code in extract-labels 
 (define (make-instruction text) 
  (list text '() '())) 
 (define (make-instruction-with-label text label) 
  (list text label '())) 
 (define (instruction-text inst) 
  (car inst)) 
 (define (instruction-label inst) 
  (cadr inst)) 
 (define (instruction-execution-proc inst) 
  (caddr inst)) 
 (define (set-instruction-execution-proc! inst proc) 
  (set-car! (cddr inst) proc)) 
  
 (define (extract-labels text) 
  (if (null? text) 
      (cons '() '()) 
          (let ((result (extract-labels (cdr text)))) 
           (let ((insts (car result)) (labels (cdr result))) 
            (let ((next-inst (car text))) 
                 (if (symbol? next-inst) 
                     (if (label-exist? labels next-inst) 
                             (error "the label has existed EXTRACT-LABELS" next-inst) 
                         (let ((insts                                                    
                                            (if (null? insts) 
                                                '() 
                                                    (cons (make-instruction-with-label  
                                                                   (instruction-text (car insts)) 
                                                       next-inst) 
                                                      (cdr insts))))) 
                                  (cons insts 
                                    (cons (make-label-entry next-inst insts) labels)))) 
                         (cons (cons (make-instruction next-inst) insts) 
                               labels))))))) 
  
 ;; change the code in execute in make-new-machine 
  (define (execute) 
         (let ((insts (get-contents pc))) 
          (if (null? insts) 
              'done 
                  (begin 
                   (if trace-on 
                       (begin 
                        (if (not (null? (instruction-label (car insts))))                      
                                (begin  
                                     (display (instruction-label (car insts))) 
                                     (newline))) 
                            (display (instruction-text (car insts))) 
                        (newline))) 
                    ((instruction-execution-proc (car insts))) 
                    (set! instruction-number (+ instruction-number 1)) 
                    (execute))))) 