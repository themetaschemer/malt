#lang racket

(require "1-reflect.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))

(define tp-tensor-equal?
  (λ (tp-actual tp-expected)
    (flat:tensor-equal? (↓ tp-actual) (↓ tp-expected))))

(require rackunit)
(define-binary-check (tp-check-tensor-equal? tp-tensor-equal? actual expected))

(include "test/test-A-equality.rkt")

(provide (rename-out
          (flat:tolerance tolerance)
          (tp-tensor-equal? tensor-equal?)
          (tp-check-tensor-equal? check-tensor-equal?)))
