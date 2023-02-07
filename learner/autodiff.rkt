#lang racket

(require "autodiff/A-autodiff.rkt")
(require "autodiff/B-prims.rkt")
(require "autodiff/D-test-helpers.rkt")
(require "autodiff/E-print.rkt")

(provide dual dual? ρ κ ∇ ∇¹ scalar? trace-print dual*)

(provide prim1 prim2)

(provide tolerance tensor-equal? check-tensor-equal?
         check-dual-equal? check-ρ-∇
         max-tensor-print-length make-printable)
