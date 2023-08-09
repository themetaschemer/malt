#lang racket
(require "tensors/0-lazy.rkt")
(require "tensors/A-equality.rkt")

(provide start-vector-manager vector-manager-report)

(provide tolerance tensor-equal? check-tensor-equal?)

(provide len ref refr)
(provide tref tlen list->tensor tensor build-tensor trefs)

(provide ext1-ρ ext2-ρ ext1-∇ ext2-∇)

(provide force/eval scalarize)

;; These will get overriden by duals
(provide tensor?)
(provide rank shape reshape size-of)

(provide force*1 force*2)
