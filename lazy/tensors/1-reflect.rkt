#lang racket
(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))
(require "c0-ast.rkt")
(require (only-in "c3-compiler.rkt"
                  compiler-cache
                  print-compiler?
                  get-compiled
                  compile-tensor))
(require (only-in "c2-interpreter.rkt" interp-racket))

(define ↓
  (lambda (tp)
    (match tp
      [(tpromise v _)
       #:when (or (flat:flat? v) (number? v))
       v]
      [(tpromise t _)
       #:when (tcomp? t)
       (let-values (((instrs data-segment) (compile-tensor t)))
         (let ((res (interp-racket instrs data-segment)))
           (set-tpromise-tensor! tp res)
           res))]
      ;; NOTE: This case runs when we use tp-scalarize to turn
      ;; the tensor to a scalar
      (_ tp))))

;; We may have to replace tp-scalarize with scalarize from flat-tensors, because
;; the ↓ used in its definition is undesirable.
(define tp-scalarize
  (λ (tp)
    (cond
      [(and (tpromise? tp) (null? (tpromise-shape tp)))
       (tp-scalarize (↓ tp))]
      [(and (flat:flat? tp) (null? (flat:flat-shape tp)))
       (vector-ref (flat:flat-store tp) 0)]
      [else tp])))

;; TODO: these force functions will be moved to the openCL runtime
(define force*1
  (λ (t f)
    (f (↓ t))))

(define force*2
  (λ (ts f)
    (let-values (((t1 t2) (ts)))
      (f (↓ t1) (↓ t2)))))

(include "test/test-1-reflect.rkt")

(provide ↓ force*1 force*2)

(provide print-compiler? compiler-cache get-compiled
         (rename-out
          (tp-scalarize scalarize)))
