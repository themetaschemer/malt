#lang racket
(require "tensors/0-vectors.rkt")
(require "tensors/1-flats.rkt")
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
                  idxs
                  scalarize))
(require (only-in "autodiff/E-print.rkt"
                  make-printable-flat
                  fake-tensor))

(provide (all-from-out "tensors/0-vectors.rkt"))
(provide (all-from-out "tensors/1-flats.rkt"))
(provide (all-from-out "tensors/B-tensor-basics.rkt"))
(provide (all-from-out "tensors/D-extend.rkt"))
(provide (all-from-out "autodiff/E-print.rkt"))
