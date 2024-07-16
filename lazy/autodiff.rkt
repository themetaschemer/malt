#lang racket

(require "autodiff/A-autodiff.rkt")
(require "autodiff/B-prims.rkt")
(require "autodiff/C-dualized-tensor-ops.rkt")
(require "autodiff/D-test-helpers.rkt")
(require "autodiff/E-print.rkt")

(provide dual dual? ρ κ ∇ ∇¹ scalar? trace-print dual* map*)
(provide prim1 prim2 ext1 ext2)
(provide (rename-out (d-rank rank)
                     (d-shape shape)
                     (d-reshape reshape)
                     (d-trefs trefs)
                     (d-tensor? tensor?)
                     (d-tlen tlen)
                     (d-ref ref)
                     (d-refr refr)))

(provide check-dual-equal? check-ρ-∇)

(provide make-printable max-tensor-print-length)
