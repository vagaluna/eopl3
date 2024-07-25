(module data-structures (lib "eopl.ss" "eopl")

  (provide (all-defined-out))

  (define end-cont
    (lambda ()
      (lambda (val)
        (begin (eopl:printf "End of computation.~%")
              (eopl:printf "This sentence should appear only once.~%")
              val))))

  (define apply-cont
    (lambda (cont val)
      (cont val)))

  ; remove-first original
  ; (define remove-first
  ;   (lambda (s los)
  ;     (if (null? los)
  ;         '()
  ;         (if (eqv? (car los) s)
  ;             (cdr los)
  ;             (cons (car los) (remove-first s (cdr los)))))))

  (define remove-first-cont
    (lambda (head saved-cont)
      (lambda (val)
        (saved-cont (cons head val)))))

  (define remove-first/k
    (lambda (s los cont)
      (if (null? los)
          (apply-cont cont '())
          (if (eqv? (car los) s)
              (apply-cont cont (cdr los))
              (remove-first/k s
                              (cdr los)
                              (remove-first-cont (car los) cont))))))
  (define remove-first
    (lambda (s los)
      (remove-first/k s los (end-cont))))


  ; list-sum original
  ; (define list-sum
  ;   (lambda (loi)
  ;     (if (null? loi)
  ;         0
  ;         (+ (car loi)
  ;            (list-sum (cdr loi))))))

  (define list-sum-cont
    (lambda (head saved-cont)
      (lambda (val)
        (saved-cont (+ head val)))))

  (define list-sum/k
    (lambda (loi cont)
      (if (null? loi)
          (apply-cont cont 0)
          (list-sum/k (cdr loi) (list-sum-cont (car loi) cont)))))

  (define list-sum
    (lambda (loi)
      (list-sum/k loi (end-cont))))

  ; occurs-free? original
  ; (define occurs-free?
  ;   (lambda (var exp)
  ;     (cond [(symbol? exp) (eqv? var exp)]
  ;           [(eqv? (car exp) 'lambda) (and (not (eqv? var (caadr exp)))
  ;                                          (occurs-free? var (caddr exp)))]
  ;           [else (or (occurs-free? var (car exp))
  ;                     (occurs-free? var (cadr exp)))])))

  (define occurs-free?-cont
    (lambda (var exp saved-cont)
      (lambda (val)
        (if val
            (apply-cont saved-cont #t)
            (occurs-free?/k var (cadr exp) saved-cont)))))

  (define occurs-free?/k
    (lambda (var exp cont)
      (cond [(symbol? exp) (apply-cont cont (eqv? var exp))]
            [(eqv? (car exp) 'lambda) (if (not (eqv? var (caadr exp)))
                                          (occurs-free?/k var (caddr exp) cont)
                                          (apply-cont cont #f))]
            [else (occurs-free?/k var (car exp) (occurs-free?-cont var exp cont))])))

  (define occurs-free?
    (lambda (var exp)
      (occurs-free?/k var exp (end-cont))))

  ; subst original

  ; (define subst-in-s-exp
  ;   (lambda (new old sexp)
  ;     (if (symbol? sexp)
  ;         (if (eqv? sexp old)
  ;             new
  ;             sexp)
  ;         (subst new old sexp))))

  ; (define subst
  ;   (lambda (new old slist)
  ;     (if (null? slist)
  ;         '()
  ;         (cons (subst-in-s-exp new old (car slist))
  ;               (subst new old (cdr slist))))))

  (define subst-cont
    (lambda (val1 saved-cont)
      (lambda (val)
        (apply-cont saved-cont (cons val1 val)))))

  (define subst-in-s-exp-cont
    (lambda (new old slist saved-cont)
      (lambda (val)
        (subst/k new old (cdr slist) (subst-cont val saved-cont)))))
  
  (define subst/k
    (lambda (new old slist cont)
      (if (null? slist)
          (apply-cont cont '())
          (subst-in-s-exp/k new old (car slist) (subst-in-s-exp-cont new old slist cont)))))

  (define subst-in-s-exp/k
    (lambda (new old sexp cont)
      (if (symbol? sexp)
          (if (eqv? sexp old)
              (apply-cont cont new)
              (apply-cont cont sexp))
          (subst/k new old sexp cont))))

  (define subst
    (lambda (new old slist)
      (subst/k new old slist (end-cont))))
)