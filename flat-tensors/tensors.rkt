#lang racket
(require "tensors/0-vectors.rkt")
(require "tensors/1-flats.rkt")
(require "tensors/A-equality.rkt")
(require "tensors/B-tensor-basics.rkt")
(require "tensors/C-tensor-ops.rkt")
(require "tensors/D-extend.rkt")

(provide start-vector-manager vector-manager-report)

(provide tolerance tensor-equal? check-tensor-equal?)

(provide len ref refr)
(provide tref tlen list->tensor tensor build-tensor trefs)

(provide ext1-ρ ext2-ρ ext1-∇ ext2-∇)

(provide flat? flat-shape flat-store flat-offset size-of strides)

;; These will get overriden by duals
(provide tensor?)
(provide rank shape reshape size-of)
