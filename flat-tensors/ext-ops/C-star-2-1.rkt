#lang racket

(require (only-in "../tensors.rkt" ext2-ρ))
(require "../autodiff.rkt")

(define *-2-1-base-ρ
  (λ (v0 i0 stride0
      v1 i1 stride1
      v-out i-out stride-out)
    (for ([i (in-range 0 stride-out)])
      (vector-set! v-out (+ i-out i)
        (* (vector-ref v0 (+ i0 i))
           (vector-ref v1 (+ i1 (modulo i stride1))))))))

(define *-2-1-base-∇
  (λ (g0 g1 v0 i0 stride0
      v1 i1 stride1
      vz iz stride-z)
    (for ([i (in-range 0 stride-z)])
      (let ((a (vector-ref v0 (+ i0 i)))
            (b (vector-ref v1 (+ i1 (modulo i stride1))))
            (z (vector-ref vz (+ iz i))))
        (vector-set! g0 (+ i0 i)
          (+ (vector-ref g0 (+ i0 i)) (* z b)))
        (vector-set! g1 (+ i1 (modulo  i stride1))
          (+ (vector-ref g1 (+ i1 (modulo i stride1))) (* z a)))))))

(define *-2-1-shape
  (λ (s t)
    s))

(define *-2-1
  (prim2 *-2-1-base-ρ *-2-1-base-∇ *-2-1-shape))

(define d*-2-1
  (ext2 *-2-1 2 1))

(define *-2-1-ρ
  (ext2-ρ *-2-1-base-ρ 2 1 *-2-1-shape))

(include "test/test-C-star-2-1.rkt")

(provide *-2-1-ρ d*-2-1)
