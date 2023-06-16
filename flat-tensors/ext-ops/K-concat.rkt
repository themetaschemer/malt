#lang racket

(require (only-in "../tensors.rkt" ext2-ρ tref ref))
(require (only-in "../autodiff.rkt" prim2 ext2))

(define concat-1-1-shape
  (λ (st su)
    (list (+ (ref st 0) (ref su 0)))))

(define concat-1-1-base-ρ
  (λ (v0 i0 stride0
      v1 i1 stride1
      v-out i-out stride-out)
    (for ([i (in-range 0 stride-out)])
      (cond
        ((< i stride0)
         (vector-set! v-out (+ i-out i) (vector-ref v0 (+ i0 i))))
        (else
         (vector-set! v-out (+ i-out i) (vector-ref v1 (+ i1 (- i stride0)))))))))

(define concat-1-1-base-∇
  (λ (g0 g1 v0 i0 stride0
      v1 i1 stride1
      vz iz stride-z)
    (for ([i (in-range 0 stride-z)])
      (cond
        ((< i stride0)
         (vector-set! g0 (+ i0 i)
           (+ (vector-ref g0 (+ i0 i))
              (vector-ref vz (+ iz i)))))
        (else
         (vector-set! g1 (+ i1 (- i stride0))
           (+ (vector-ref g1 (+ i1 (- i stride0)))
              (vector-ref vz (+ iz i)))))))))

(define concat-1-1
  (prim2 concat-1-1-base-ρ concat-1-1-base-∇ concat-1-1-shape))

(define d-concat
  (ext2 concat-1-1 1 1))

(define concat-ρ
  (ext2-ρ concat-1-1-base-ρ 1 1 concat-1-1-shape))

(include "test/test-K-concat.rkt")

(provide concat-1-1 d-concat concat-ρ)
