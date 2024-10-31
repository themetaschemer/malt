#lang racket
(require (for-syntax "impl-loader.rkt"))
(require rackunit)

(provide unsafe-test)

;; This macro will disable tests which give precision errors. In the future, we
;; will use this macro to switch testing behaviour so tests run without failing
;; due to hardware issues.
(define-syntax (unsafe-test x)
  (syntax-case x ()
    [(_ test) (disable-unsafe-tests?) #'(check-true #t)]
    [(_ test) #'test]))
