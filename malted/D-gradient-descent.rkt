#lang racket

(require "../base.rkt")
(require "A-core.ss")

(define revise
  (λ (f revs theta)
    (cond
      ((zero? revs) theta)
      (else (revise f (sub1 revs) (f theta))))))

(declare-hyper revs)
(declare-hyper alpha)

;;TODO: abstract away the lazy implementation specific ↓ using a with-aspect

;; For lazy implementation
#;
(define-syntax with-aspect
  (syntax-rules ()
    [(_ 'gd-update f)
     (lambda (pa g) (map* ↓ (f pa g)))]
    [(_ _ f) f]))

;; For other implementations
#;
(define-syntax with-aspect
  (syntax-rules ()
    [(_ _ f)
     f]))

(define gradient-descent
  (lambda (inflate deflate update)
    (λ (obj theta)
      (let ((ctr 0))
        (let ((f (λ (big-theta)
                   (map #;(with-aspect 'gd-update update)
                        (lambda (pa g) (map* ↓ (update pa g)))
                     big-theta
                     (gradient-of obj
                       (map deflate big-theta))))))
          (map deflate
            (revise f revs
              (map inflate theta))))))))

(include "test/test-D-gradient-descent.rkt")

(provide gradient-descent revise)
