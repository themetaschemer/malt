#lang racket

(require "A-autodiff.rkt")
(require "../tensors/0-lazy.rkt")
(require "../../flat-tensors/ext-impl.rkt")

(define max-tensor-print-length (make-parameter 5))

(define make-printable
  (λ (y [max-length (max-tensor-print-length)])
    (cond
      ((dual? y) (make-printable (ρ y)))
      ((tpromise? y)
       (make-printable (tp-force y) max-length))
      ((flat? y) (make-printable-flat y max-length))
      ((list? y)
       (map (λ (le) (make-printable le max-length)) y))
      ((vector? y)
       (vector-map (λ (ve) (make-printable ve max-length)) y))
      (else y))))

(include "test/test-E-print.rkt")

(provide max-tensor-print-length
         make-printable)
