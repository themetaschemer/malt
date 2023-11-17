#lang racket

(require "../base.rkt")
;; TODO: This is not implementation independent. Figure out a fix
(require (only-in "../lazy/tensors.rkt" ↓))

(define dot-product
  (λ (w t)
    (sum
      (* w t))))

(define dot-product-2-1
  (λ (w t)
    (sum
      (*-2-1 w t))))

(include "test/test-A-core.rkt")

(provide dot-product dot-product-2-1 ↓)
