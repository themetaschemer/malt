#lang racket

(require "../base.rkt")

(define gradient-of ∇)

(define dot-product
  (λ (w t)
    (sum
      (* w t))))

(define dot-product-2-1
  (λ (w t)
    (sum
      (*-2-1 w t))))

(include "test/test-A-core.rkt")

(provide gradient-of ∇ dot-product-2-1 dot-product)
