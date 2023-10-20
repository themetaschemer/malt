#lang racket

(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))

(struct ext2-∇-result (res) #:mutable #:transparent)

(define ext2-∇-forcer
  (λ (fᵈ r0 r1 shape-fn t0 t1 z out0 out1)
    (let* ((f0 (ensure-flat t0))
           (f1 (ensure-flat t1))
           (fz (ensure-flat z))

           (s0 (flat-shape f0))
           (sf0 (min-shape r0 s0))
           (stride0 (flat:size-of sf0))

           (s1 (flat-shape t1))
           (sf1 (min-shape r1 s1))
           (stride1 (flat:size-of sf1))

           (sf-z (shape-fn sf0 sf1))
           (stride-z (flat:size-of sf-z))

           (v0 (flat-store f0))
           (v1 (flat-store f1))
           (vz (flat-store fz))

           (off0 (flat-offset f0))
           (off1 (flat-offset f1))
           (offz (flat-offset fz)))
      (ext2-shapes
       s0 s1 r0 r1 sf-z
       (λ (sz size-z q0 q1 strides)
         (let ((g0 (new-vec (flat:size-of
                             s0)
                            0.0))
               (g1 (new-vec (flat:size-of
                             s1)
                            0.0)))
           (for ([iz (in-range
                      0
                      size-z
                      stride-z)])
             (let-values (((i0 i1)
                           (idxs
                            strides
                            iz
                            off0
                            off1)))
               (fᵈ g0 g1 v0 i0
                   stride0
                   v1 i1
                   stride1
                   vz
                   (+ offz iz)
                   stride-z)))
           (set-ext2-∇-result-res! out0
                     (scalarize (flat s0 g0 0)))
           (set-ext2-∇-result-res! out1
                     (scalarize (flat s1 g1 0)))))))))

(define rt:trefs
  (λ (ft b)
    (cond
      ((= (flat:rank b) 1) (flat:trefs ft (vector->list (flat-store b))))
      (else (error 'trefs-err "~a should be a tensor¹" b)))))

(define data-segment
  (make-parameter #f))

(define data-segment-ref
  (λ (i)
    (vector-ref (data-segment) i)))

(define-namespace-anchor a)
(define runtime
  ;;TODO explicitly declare the names being included in this namespace
  (namespace-anchor->namespace a))

(provide runtime flat? flat:build-tensor flat:list->tensor
         flat:tref rt:trefs (struct-out ext2-∇-result)
         ext2-∇-forcer scalarize flat-ext1-∇ ensure-flat flat-ext2-ρ
         flat flat-store flat-offset flat-ext1-ρ)