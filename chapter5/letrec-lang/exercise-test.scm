#lang racket
; (module exercise-test (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "exercise.scm")           ; for value-of-program
  
  (provide (all-defined-out))
  ; (provide (all-from-out "exercise.scm"))
  ; (provide (all-from-out "lang.scm"))
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal

  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  (define test-5.1
    (lambda ()
      (run "let2 xxx=87 yyy=30 in -(xxx,yyy)")))
  
; )


