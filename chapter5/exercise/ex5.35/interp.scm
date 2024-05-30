(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)
  (provide trace-apply-procedure)

  (define trace-apply-procedure (make-parameter #f))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


  (define-datatype continuation continuation?
    (end-cont)                          ; []
    (diff1-cont                       ; cont[(- [] (value-of e2 env))]
      (exp2 expression?)
      (env environment?)
      (cont continuation?)
      (tryctx continuation?))
    (diff2-cont                         ; cont[(- val1 [])]
      (val1 expval?)
      (cont continuation?)
      (tryctx continuation?))
    (unop-arg-cont
      (unop unary-op?)
      (cont continuation?)
      (tryctx continuation?))
    (if-test-cont
      (exp2 expression?)
      (exp3 expression?)
      (env environment?)
      (cont continuation?)
      (tryctx continuation?))
    (rator-cont            ; cont[(apply-proc [] (value-of rand env))]
      (rand expression?)
      (env environment?)
      (cont continuation?)
      (tryctx continuation?))
    (rand-cont                          ; cont[(apply-proc val1 [])]
      (val1 expval?)
      (cont continuation?)
      (tryctx continuation?))
    (try-cont
      (var symbol?)
      (handler-exp expression?)
      (env environment?)
      (cont continuation?))
    (raise1-cont
      (saved-cont continuation?)
      (tryctx continuation?))
    )

(define get-tryctx
  (lambda (cont)
    (cases continuation cont
        (end-cont () cont)
        (diff1-cont (exp2 saved-env saved-cont tryctx) tryctx)
        (diff2-cont (val1 saved-cont tryctx) tryctx)
        (unop-arg-cont (unop saved-cont tryctx) tryctx)
        (if-test-cont (exp2 exp3 env saved-cont tryctx) tryctx)
        (rator-cont (rand saved-env saved-cont tryctx) tryctx)
        (rand-cont (val1 saved-cont tryctx) tryctx)
        (try-cont (var handler-exp saved-env saved-cont) cont)
        (raise1-cont (saved-cont tryctx) tryctx)
    )))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (body)
          (value-of/k body (init-env) (end-cont))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 173
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp

        (const-exp (num) (apply-cont cont (num-val num)))

        (const-list-exp (nums)
          (apply-cont cont
            (list-val (map num-val nums))))

        (var-exp (var) (apply-cont cont (apply-env env var)))

        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont (get-tryctx cont))))

        (unop-exp (unop exp1)
          (value-of/k exp1 env
            (unop-arg-cont unop cont (get-tryctx cont))))

        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont (get-tryctx cont))))

        (proc-exp (var body)
          (apply-cont cont
            (proc-val
              (procedure var body env))))

        (call-exp (rator rand)
          (value-of/k rator env
            (rator-cont rand env cont (get-tryctx cont))))

        ;; make let a macro, because I'm too lazy to add the extra
        ;; continuation
        (let-exp (var exp1 body)
          (value-of/k
            (call-exp (proc-exp var body) exp1)
            env
            cont))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k
            letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))

        (try-exp (exp1 var handler-exp)
          (value-of/k exp1 env
            (try-cont var handler-exp env cont)))

        (raise-exp (exp1)
          (value-of/k exp1 env
            (raise1-cont cont (get-tryctx cont)))))))

  ;; apply-cont : continuation * expval -> final-expval

  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          (begin
            (eopl:printf
              "End of computation.~%")
            val))
        (diff1-cont (exp2 saved-env saved-cont tryctx)
          (value-of/k exp2 saved-env (diff2-cont val saved-cont tryctx)))
        (diff2-cont (val1 saved-cont tryctx)
          (let ((n1 (expval->num val1))
                (n2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- n1 n2)))))
        (unop-arg-cont (unop cont tryctx)
          (apply-cont cont
            (apply-unop unop val)))
        (if-test-cont (exp2 exp3 env cont tryctx)
          (if (expval->bool val)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))
        (rator-cont (rand saved-env saved-cont tryctx)
          (value-of/k rand saved-env
            (rand-cont val saved-cont tryctx)))
        (rand-cont (val1 saved-cont tryctx)
          (let ((proc (expval->proc val1)))
            (apply-procedure proc val saved-cont)))
        ;; the body of the try finished normally-- don't evaluate the handler
        (try-cont (var handler-exp saved-env saved-cont)
          (apply-cont saved-cont val))
        ;; val is the value of the argument to raise
        (raise1-cont (saved-cont tryctx)
          ;; we put the short argument first to make the trace more readable.
          (apply-handler val tryctx))
        )))
      
  ;; apply-handler : ExpVal * Cont -> FinalAnswer
  (define apply-handler
    (lambda (val cont)
      (cases continuation cont
        (try-cont (var handler-exp saved-env saved-cont)
          (value-of/k handler-exp
            (extend-env var val saved-env)
            saved-cont))
        (end-cont () (eopl:error 'apply-handler "uncaught exception!"))
        (else (eopl:error 'apply-handler "tryctx is not try-cont or end-cont")))
    )
  )


  ;; apply-procedure : procedure * expval * cont -> final-expval

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))


  (define apply-unop
    (lambda (unop val)
      (cases unary-op unop
        (null?-unop ()
          (bool-val
            (null? (expval->list val))))
        (car-unop ()
          (car (expval->list val)))
        (cdr-unop ()
          (list-val (cdr (expval->list val))))
        (zero?-unop ()
          (bool-val
            (zero? (expval->num val)))))))


  ;; to get the detailed trace:
  ;; (trace value-of/k apply-cont apply-handler)

  )
