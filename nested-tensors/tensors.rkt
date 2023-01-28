#lang racket
(require "tensors/A-equality.rkt")
(require "tensors/B-tensor-basics.rkt")
(require "tensors/C-tensor-ops.rkt")
(require "tensors/D-extend.rkt")

(provide tolerance tensor-equal? check-tensor-equal?)

(provide len ref refr)
(provide tref tlen tmap list->tensor tensor build-tensor trefs)


(provide ext1-ρ ext2-ρ ext1-∇ ext2-∇)

;; These will get overriden by duals
(provide tensor?)
(provide rank shape reshape size-of)
