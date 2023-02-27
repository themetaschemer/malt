#lang racket

(require "learner/no-overrides.rkt")
(require "ports.rkt")
(printable-maker make-printable)

(define dot-product-1-1
  (λ (t0 t1)
    (sum-1
      (d* t0 t1))))

(provide dot-product-1-1)

(provide (all-from-out "learner/no-overrides.rkt"))
