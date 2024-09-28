#lang racket
(require "tensors/0-vectors.rkt")
(require "tensors/1-flats.rkt")
(require (only-in "tensors/2-acc-runtime.rkt"
                  ext2-∇-kernel/name
                  run-prim2-∇!))
(require (only-in "tensors/B-tensor-basics.rkt"
                  merge-flats))
(require (only-in "tensors/D-extend.rkt"
                  merge-shapes
                  min-shape
                  ext2-shapes
                  flat-ext1-∇
                  flat-ext1-ρ
                  flat-ext2-ρ
                  functional->preallocated-1-ρ
                  functional->preallocated-1-∇
                  functional->preallocated-2-ρ
                  functional->preallocated-2-∇
                  functional->preallocated-1-ρ-acc
                  functional->preallocated-1-∇-acc
                  functional->preallocated-2-ρ-acc
                  functional->preallocated-2-∇-acc
                  idxs
                  scalarize
                  ensure-flat))
(require (only-in "autodiff/B-prims.rkt"
                  apply-flat-ρ-fn-1
                  apply-flat-ρ-fn-2
                  apply-flat-∇-fn-1
                  apply-flat-∇-fn-2))
(require (only-in "autodiff/E-print.rkt"
                  make-printable-flat
                  fake-tensor))

(provide (all-from-out "tensors/0-vectors.rkt"))
(provide (all-from-out "tensors/1-flats.rkt"))
(provide (all-from-out "tensors/2-acc-runtime.rkt"))
(provide (all-from-out "tensors/B-tensor-basics.rkt"))
(provide (all-from-out "tensors/D-extend.rkt"))
(provide (all-from-out "autodiff/B-prims.rkt"))
(provide (all-from-out "autodiff/E-print.rkt"))
