#lang racket

(require ffi/vector)
(require "../../impl-loader.rkt")
(require "../../accelerated-tensors/ext-impl.rkt")
(require (prefix-in acc: "../../accelerated-tensors/tensors.rkt"))

(struct ext2-∇-result (res) #:mutable #:transparent)

(define ext2-∇-forcer!
  (λ (fᵈ fᵈ-acc fᵈ-sign r0 r1 shape-fn t0 t1 z out-idx0 out-idx1)
    (let* ((f0 (ensure-flat t0))
           (f1 (ensure-flat t1))
           (fz (ensure-flat z))

           (s0 (flat-shape f0))
           (sf0 (min-shape r0 s0))
           (stride0 (acc:size-of sf0))

           (s1 (flat-shape t1))
           (sf1 (min-shape r1 s1))
           (stride1 (acc:size-of sf1))

           (sf-z (shape-fn sf0 sf1))
           (stride-z (acc:size-of sf-z))

           (v0 (flat-store f0))
           (v1 (flat-store f1))
           (vz (flat-store fz))

           (off0 (flat-offset f0))
           (off1 (flat-offset f1))
           (offz (flat-offset fz)))
      (ext2-shapes
       s0 s1 r0 r1 sf-z
       (λ (sz size-z q0 q1 strides)
         (let ((g0 (new-vec (acc:size-of s0) 0.0))
               (g1 (new-vec (acc:size-of s1) 0.0)))
           (cond
              ((accelerate?)
               (let-values (((kernel-code kernel-name)
                             (ext2-∇-kernel/name fᵈ-acc fᵈ-sign strides s0 s1 r0 r1 sz
                                                 (length sf-z))))
                 (run-prim2-∇! kernel-code kernel-name
                               g0 g1
                               v0 off0 (acc:size-of s0) stride0
                               v1 off1 (acc:size-of s1) stride1
                               vz offz size-z stride-z)))
              (else
               (for ([iz (in-range 0 size-z stride-z)])
                 (let-values (((i0 i1) (idxs strides iz off0 off1)))
                   (fᵈ g0 g1 v0 i0 stride0 v1 i1 stride1 vz (+ offz iz) stride-z)))))
           (when out-idx0
             (data-segment-set! out-idx0 (scalarize (flat s0 g0 0))))
           (when out-idx1
             (data-segment-set! out-idx1 (scalarize (flat s1 g1 0))))))))))

(define prim2-∇-forcer!
  (λ (fᵈ fᵈ-acc fᵈ-sign shape-fn t0 t1 z out-idx0 out-idx1)
    (let* ((in-shape-a (flat-shape t0))
           (in-size-a (size-of in-shape-a))
           (in-shape-b (flat-shape t1))
           (in-size-b (size-of in-shape-b))
           (out-shape (shape-fn in-shape-a in-shape-b))
           (out-size (size-of out-shape)))
      (let ((g0 (new-vec in-size-a 0.0))
            (g1 (new-vec in-size-b 0.0)))
        (cond
          ((null? out-shape)
           (let ((v-z (new-vec 1 z)))
             (fᵈ g0 g1
               (flat-store t0) (flat-offset t0) in-size-a
               (flat-store t1) (flat-offset t1) in-size-b
               v-z 0 1)))
          (else
           (fᵈ g0 g1
             (flat-store t0) (flat-offset t0) in-size-a
             (flat-store t1) (flat-offset t1) in-size-b
             (flat-store z) (flat-offset z) out-size)))
        (when out-idx0
             (data-segment-set! out-idx0 (scalarize (flat in-shape-a g0 0))))
        (when out-idx1
          (data-segment-set! out-idx1 (scalarize (flat in-shape-b g1 0))))))))

(define rt:trefs
  (λ (ft b)
    (cond
      ((= (acc:rank b) 1) (acc:trefs ft (map inexact->exact (f32vector->list (flat-store b)))))
      (else (error 'trefs-err "~a should be a tensor¹" b)))))

(define data-segment
  (make-parameter #f))

(define data-segment-set!
  (λ (i v)
    (vector-set! (data-segment) i v)))

(define data-segment-ref
  (λ (i)
    (vector-ref (data-segment) i)))

(define-namespace-anchor a)
(define runtime
  (namespace-anchor->namespace a))

(provide runtime flat? acc:build-tensor acc:list->tensor
         acc:tref rt:trefs (struct-out ext2-∇-result) set-ext2-∇-result-res!
         ext2-∇-forcer! scalarize flat-ext1-∇ ensure-flat flat-ext2-ρ
         flat flat-store flat-offset flat-ext1-ρ data-segment data-segment-ref
         apply-flat-ρ-fn-1 apply-flat-ρ-fn-2 apply-flat-∇-fn-1 apply-flat-∇-fn-2
         prim2-∇-forcer!)
