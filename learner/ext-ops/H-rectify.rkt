#lang racket

(require "../tensors.rkt")
(require (only-in (rename-in "B-comparators.ss" (<-0-0 <)) <))

(define rectify-0
  (λ (x)
    (cond
      ((< x 0.0) 0.0)
      (else x))))

(define rectify
  (ext1 rectify-0 0))

(include "test/test-H-rectify.rkt")

(provide rectify-0 rectify)
