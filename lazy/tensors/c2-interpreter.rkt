#lang racket

(require "c0-ast.rkt")
(require (only-in "c1-racket-runtime.rkt"
                  runtime flat? acc:build-tensor acc:list->tensor
                  set-ext2-∇-result-res! acc:tref rt:trefs ext2-∇-result-res
                  ext2-∇-forcer! scalarize flat-ext1-∇ ensure-flat flat-ext2-ρ
                  flat flat-store flat-offset flat-ext1-ρ data-segment
                  apply-flat-ρ-fn-1 apply-flat-ρ-fn-2 apply-flat-∇-fn-1 apply-flat-∇-fn-2
                  data-segment-ref prim2-∇-forcer!))

(define interp-tcomp
  (λ (tc env)
    (match tc
      [(tcomp-list->tensor lst)
       (let ((eval-list
              (for/list ((arg lst))
                (cond
                  ((tpromise? arg) (interp-tpromise arg env))
                  ((number? arg) arg)
                  (else (error 'interp-list->tensor "Unexpected: ~a" arg))))))
         (acc:list->tensor eval-list))]
      [(tcomp-tref tp (and i (tcomp-ds-ref _)))
       (acc:tref (interp-tpromise tp env)
                  (interp-tcomp i env))]
      [(tcomp-trefs tp (and b (tcomp-ds-ref _)))
       (rt:trefs (interp-tpromise tp env)
                 (interp-tcomp b env))]
      [(tcomp-ext2-∇ fᵈ fᵈ-acc f-sign r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       (let ((t0-instrs (interp-tpromise tp-t0 env))
             (t1-instrs (interp-tpromise tp-t1 env))
             (z-instrs (interp-tpromise tp-z env)))
         (cond
           ((and (eqv? i 0)
                 (not (tcomp-ds-ref-index (ext2-∇-result-res out0))))
            (set-ext2-∇-result-res! out0 (tcomp-ds-ref (current-ds-ref-index)))
            (current-ds-ref-index (add1 (current-ds-ref-index))))
           ((and (eqv? i 1)
                 (not (tcomp-ds-ref-index (ext2-∇-result-res out1))))
            (set-ext2-∇-result-res! out1 (tcomp-ds-ref (current-ds-ref-index)))
            (current-ds-ref-index (add1 (current-ds-ref-index)))))
         (let* ((out-idx0 (tcomp-ds-ref-index (ext2-∇-result-res out0)))
                (out-idx1 (tcomp-ds-ref-index (ext2-∇-result-res out1)))
                (index (if (zero? i) out-idx0 out-idx1))
                (v (data-segment-ref index)))
           (cond
             ((eqv? v 'uncalculated)
              (ext2-∇-forcer! fᵈ fᵈ-acc f-sign r0 r1 shape-fn
                              t0-instrs t1-instrs
                              z-instrs out-idx0 out-idx1)
              (data-segment-ref index))
             (else v))))]
      [(tcomp-ext1-∇ tp zp f f-acc f-sign m shape-fn)
       (scalarize
        (flat-ext1-∇ f f-acc m shape-fn f-sign
                     (ensure-flat (interp-tpromise tp env))
                     (ensure-flat (interp-tpromise zp env))))]
      [(tcomp-ext2-ρ-scalar f f-acc _ tp-t tp-u)
       (f (interp-tpromise tp-t env) (interp-tpromise tp-u env))]
      [(tcomp-ext2-ρ tp-t tp-u f f-acc f-sign m n shape-fn)
       (scalarize
        (flat-ext2-ρ f f-acc m n shape-fn f-sign
                     (ensure-flat (interp-tpromise tp-t env))
                     (ensure-flat (interp-tpromise tp-u env))))]
      [(tcomp-ext1-ρ-scalar f f-acc _ tp)
       (f (interp-tpromise tp env))]
      [(tcomp-ext1-ρ f f-acc f-sign m shape-fn tp)
       (scalarize
        (flat-ext1-ρ f f-acc m shape-fn f-sign
                     (ensure-flat (interp-tpromise tp env))))]
      [(tcomp-prim1-ρ f f-acc sign shape-fn tp)
       (apply-flat-ρ-fn-1 f (interp-tpromise tp) shape-fn)]
      [(tcomp-prim2-ρ f f-acc sign shape-fn tp-t tp-u)
       (apply-flat-ρ-fn-2 f (interp-tensor tp-t) (interp-tpromise tp-u) shape-fn)]
      [(tcomp-prim1-∇ f f-acc sign shape-fn tp zp)
       (apply-flat-∇-fn-1 f (interp-tpromise tp) (scalarize (interp-tpromise zp)) shape-fn)]
      [(tcomp-prim2-∇ fᵈ fᵈ-acc f-sign shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       (let ((t0-instrs (interp-tpromise tp-t0 env))
             (t1-instrs (interp-tpromise tp-t1 env))
             (z-instrs (interp-tpromise tp-z env)))
         (cond
           ((and (eqv? i 0)
                 (not (tcomp-ds-ref-index (ext2-∇-result-res out0))))
            (set-ext2-∇-result-res! out0 (tcomp-ds-ref (current-ds-ref-index)))
            (current-ds-ref-index (add1 (current-ds-ref-index))))
           ((and (eqv? i 1)
                 (not (tcomp-ds-ref-index (ext2-∇-result-res out1))))
            (set-ext2-∇-result-res! out1 (tcomp-ds-ref (current-ds-ref-index)))
            (current-ds-ref-index (add1 (current-ds-ref-index)))))
         (let* ((out-idx0 (tcomp-ds-ref-index (ext2-∇-result-res out0)))
                (out-idx1 (tcomp-ds-ref-index (ext2-∇-result-res out1)))
                (index (if (zero? i) out-idx0 out-idx1))
                (v (data-segment-ref index)))
           (cond
             ((eqv? v 'uncalculated)
              (prim2-∇-forcer! fᵈ f-sign shape-fn
                               t0-instrs t1-instrs
                               z-instrs out-idx0 out-idx1)
              (data-segment-ref index))
             (else v))))]
      [(tcomp-reshape s tp)
       (let ([interp-tp (interp-tpromise tp env)])
         (flat s (flat-store interp-tp) (flat-offset interp-tp)))]
      [(tcomp-let lhs rhs body)
       (interp-tpromise
        body
        (cons
         (cons lhs
               (interp-tpromise rhs env))
         env))]
      [(tcomp-var name)
       (cond
         ((assv name env)
          =>
          (λ (p) (cdr p)))
         (else (error 'interpret-free "Free variable: ~a" name)))]
      [(tcomp-ds-ref #f)
       (let ([out (data-segment-ref (current-ds-ref-index))])
         (current-ds-ref-index (add1 (current-ds-ref-index)))
         out)]
      [(tcomp-ds-ref index)
       ;; This case is run only for languages where the tcomp-ds-ref indices are
       ;; generated by the generate-ds-refs pass.
       (data-segment-ref index)])))

(define interp-tpromise
  (λ (t env)
    (match t
      [(tpromise tc _ _ _) (interp-tcomp tc env)])))

(define current-ds-ref-index (make-parameter #f))
(define interp-tensor
  (λ (tp)
    (parameterize ([current-ds-ref-index 0]
                   [data-segment (dst->data-segment (tpromise-dst tp))])
      (interp-tpromise tp '()))))

(define interp-racket
  (lambda (instrs ds)
    (parameterize ((data-segment ds))
      (eval instrs runtime))))

(include "test/test-c2-interpreter.rkt")
(provide interp-racket interp-tensor)
