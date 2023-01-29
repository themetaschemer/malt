#lang racket

(require (only-in "../tensors.rkt" ext1-ρ))
(require "../autodiff.rkt")

(define sum-1-ρ
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    (vector-set! v-out i-out
      (for/fold ([sum 0.0]) ([i (in-range i0 (+ i0 stride0))])
        (+ sum (vector-ref v0 i))))))

(define sum-1-∇
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    (let ((z (vector-ref vz iz)))
      (for ([i (in-range i0 (+ i0 stride0))])
        (vector-set! g0 i
          (+ (vector-ref g0 i) z))))))

(define sum-shape
  (λ (st)
    (refr st 1)))

(define sum-1
  (prim1 sum-1-ρ sum-1-∇ sum-shape))

(define d-sum
  (ext1 sum-1 1))

(define sum-ρ
  (ext1-ρ sum-1-ρ 1 sum-shape))

(provide d-sum sum-ρ)

(define sum-cols-2-ρ
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    (for ((i (in-range 0 stride-out)))
      (vector-set! v-out (+ i i-out)
        (for/fold ([sum 0.0]) ([j (in-range i0 (+ i0 stride0) stride-out)])
          (+ sum (vector-ref v0 (+ j i))))))))

(define sum-cols-2-∇
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    (for ((i (in-range 0 stride-z)))
      (for ([j (in-range i0 (+ i0 stride0) stride-z)])
        (vector-set! g0 (+ i j)
          (+ (vector-ref g0 (+ i j)) (vector-ref vz (+ i iz))))))))

(define sum-cols-shape
  (λ (s)
    (refr s 1)))

(define sum-cols-2
  (prim1 sum-cols-2-ρ sum-cols-2-∇ sum-cols-shape))

(define d-sum-cols
  (ext1 sum-cols-2 2))

(define sum-cols-ρ
  (ext1-ρ sum-cols-2-ρ 2 sum-cols-shape))

(include "test/test-D-sum.rkt")

(provide sum-1 d-sum-cols sum-cols-ρ)
