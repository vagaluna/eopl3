(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)
  (provide trace-apply-procedure)

  (define trace-apply-procedure (make-parameter #f))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
  (define end-cont
    (lambda ()
      (lambda (val)
        (begin (eopl:printf "End of computation.~%")
               val))))

  (define diff1-cont
    (lambda (exp2 env cont handler)
      (lambda (val)
        (value-of/k exp2 env (diff2-cont val cont) handler))))

  (define diff2-cont
    (lambda (val1 cont handler)
      (lambda (val)
        (let ((n1 (expval->num val1))
              (n2 (expval->num val)))
            (apply-cont cont
              (num-val (- n1 n2)))))))

  (define unop-arg-cont
    (lambda (unop cont)
      (lambda (val)
        (apply-cont cont
            (apply-unop unop val)))))

  (define if-test-cont
    (lambda (exp2 exp3 env cont handler)
      (lambda (val)
        (if (expval->bool val)
            (value-of/k exp2 env cont handler)
            (value-of/k exp3 env cont handler)))))

  (define rator-cont
    (lambda (rand env cont handler)
      (lambda (val)
        (value-of/k rand env
                    (rand-cont val cont handler)
                    handler))))

  (define rand-cont
    (lambda (val1 cont handler)
      (lambda (val)
        (let ((proc (expval->proc val1)))
            (apply-procedure proc val cont handler)))))

  (define raise1-cont
    (lambda (handler)
      (lambda (val)
        (apply-handler handler val))))

  (define end-handler
    (lambda ()
      (lambda (val)
        (eopl:error 'apply-handler "uncaught exception!"))))

  (define try-handler
    (lambda (var handler-exp env cont handler)
      (lambda (val)
        (value-of/k handler-exp
            (extend-env var val env)
            cont
            handler))))


  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (body)
          (value-of/k body (init-env) (end-cont) (end-handler))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 173
  (define value-of/k
    (lambda (exp env cont handler)
      (cases expression exp

        (const-exp (num) (apply-cont cont (num-val num)))
        (const-list-exp (nums)
          (apply-cont cont
            (list-val (map num-val nums))))

        (var-exp (var) (apply-cont cont (apply-env env var)))

        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont handler)))

        (unop-exp (unop exp1)
          (value-of/k exp1 env
                      (unop-arg-cont unop cont)
                      handler))

        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
                      (if-test-cont exp2 exp3 env cont handler)
                      handler))

        (proc-exp (var body)
          (apply-cont cont
            (proc-val
              (procedure var body env))))

        (call-exp (rator rand)
          (value-of/k rator env
                      (rator-cont rand env cont handler)
                      handler))

        ;; make let a macro, because I'm too lazy to add the extra
        ;; continuation
        (let-exp (var exp1 body)
          (value-of/k
            (call-exp (proc-exp var body) exp1)
            env
            cont
            handler))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k
            letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont
            handler))

        (try-exp (exp1 var handler-exp)
          (value-of/k exp1 env
                      cont
                      (try-handler var handler-exp env cont handler)))

        (raise-exp (exp1)
          (value-of/k exp1 env
            (raise1-cont handler)
            handler)))))

  ;; apply-cont : continuation * expval -> final-expval

  (define apply-cont
    (lambda (cont val)
      (cont val)))
      
  ;; apply-handler : ExpVal * Cont -> FinalAnswer
  (define apply-handler
    (lambda (handler val)
      (handler val)))


  ;; apply-procedure : procedure * expval * cont -> final-expval

  (define apply-procedure
    (lambda (proc1 arg cont handler)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont
            handler)))))


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
