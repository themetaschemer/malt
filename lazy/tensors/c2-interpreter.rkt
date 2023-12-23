#lang racket

(require "c0-ast.rkt")
(require (only-in "c1-racket-runtime.rkt"
                  runtime flat? flat:build-tensor flat:list->tensor
                  flat:tref rt:trefs ext2-∇-result-res ext2-∇-forcer
                  scalarize flat-ext1-∇ ensure-flat flat-ext2-ρ flat flat-store
                  flat-offset flat-ext1-ρ data-segment))

(define interp-tensor-tcomp
  (λ (tc env ds)
    (match tc
      [(tcomp-list->tensor lst)
       (let ((eval-list
              (for/list ((arg lst))
                (interp-tensor-expr arg env ds))))
         (flat:list->tensor eval-list))]
      [(tcomp-tref tp i)
       (flat:tref (interp-tensor-expr tp env ds)
                  (interp-tensor-expr i env ds))]
      [(tcomp-trefs tp b)
       (rt:trefs (interp-tensor-expr tp env ds)
                 (interp-tensor-expr b env ds))]
      [(tcomp-ext2-∇ fᵈ _ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       ;; TODO: fix this case because we now use the data segment rather than
       ;; ext2-∇-result for output
       (let* ([b (if (zero? i) out0 out1)]
              [v (ext2-∇-result-res b)])
         (cond
           ((eqv? v 'uncalculated)
            (ext2-∇-forcer fᵈ r0 r1 shape-fn
                           (interp-tensor-expr tp-t0 env ds)
                           (interp-tensor-expr tp-t1 env ds)
                           (interp-tensor-expr tp-z env ds)
                           out0 out1)
            (ext2-∇-result-res b))
           (else v)))]
      [(tcomp-ext1-∇ tp zp f _ m shape-fn)
       (scalarize
        (flat-ext1-∇ f m shape-fn
                     (ensure-flat (interp-tensor-expr tp env ds))
                     (ensure-flat (interp-tensor-expr zp env ds))))]
      [(tcomp-ext2-ρ-scalar f _ tp-t tp-u)
       (f (interp-tensor-expr tp-t env ds) (interp-tensor-expr tp-u env ds))]
      [(tcomp-ext2-ρ tp-t tp-u f _ m n shape-fn)
       (scalarize
        (flat-ext2-ρ f m n shape-fn
                     (ensure-flat (interp-tensor-expr tp-t env ds))
                     (ensure-flat (interp-tensor-expr tp-u env ds))))]
      [(tcomp-ext1-ρ-scalar f _ tp)
       (f (interp-tensor-expr tp env ds))]
      [(tcomp-ext1-ρ f _ m shape-fn tp)
       (scalarize
        (flat-ext1-ρ f m shape-fn
                     (ensure-flat (interp-tensor-expr tp env ds))))]
      [(tcomp-reshape s tp)
       (flat s
             (flat-store (interp-tensor-expr tp env ds))
             (flat-offset (interp-tensor-expr tp env ds)))]
      [(tcomp-let lhs rhs body)
       (interp-tensor-expr
        body
        (cons
         (cons lhs
               (interp-tensor-expr rhs env ds))
         env)
        ds)]
      [(tcomp-var name)
       (cond
         ((assv name env)
          =>
          (λ (p) (cdr p)))
         (else (error 'interpret-free "Free variable: ~a" name)))]
      [(tcomp-ds-ref index)
       (vector-ref ds index)])))

(define interp-tensor-expr
  (λ (t env ds)
    (match t
      [(tpromise tc _ _ _) (interp-tensor-expr tc env ds)]
      [v #:when (or (flat? v) (pair? v) (number? v)) v]
      [(tcomp) (interp-tensor-tcomp t env ds)])))

(define interp-tensor
  (λ (t ds)
    (interp-tensor-expr t '() ds)))

(define interp-racket
  (lambda (instrs ds)
    (parameterize ((data-segment ds))
      (eval instrs runtime))))

(provide interp-racket interp-tensor)
