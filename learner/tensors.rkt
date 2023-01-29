#lang racket

(require "tensors/0-duals.rkt")
(require "tensors/B-tensor-basics.rkt")
(require "tensors/C-tensor-ops.rkt")
(require "tensors/D-extend.rkt")

(provide dual? dual dual* ρ κ scalar? end-of-chain)

(provide len ref refr)
(provide tref tlen tmap list->tensor tensor build-tensor trefs)

(provide ext1 ext2)

(provide tensor?)
(provide rank shape reshape size-of)
