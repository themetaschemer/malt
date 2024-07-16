#lang racket

(require (only-in "../tensors.rkt" ext1-ρ))
(require "../autodiff.rkt")

(define argmax-1-ρ
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    (vector-set! v-out i-out
      (for/fold ([max 0.0]
                 [max-i -1] #:result max-i)
          ([i (in-range i0 (+ i0 stride0))])
        (let ((v (vector-ref v0 i)))
          (cond
            ((> v max) (values v (+ (- i i0) 0.0)))
            (else (values max max-i))))))))

(define argmax-1-∇
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    (let ((z (vector-ref vz iz)))
      (for ([i (in-range i0 (+ i0 stride0))])
        (vector-set! g0 i 0.0)))))

(define argmax-shape
  (λ (st)
    '()))

(define argmax-1
  (prim1 argmax-1-ρ argmax-1-∇ argmax-shape))

(define d-argmax
  (ext1 argmax-1 1))

(define argmax-ρ
  (ext1-ρ argmax-1-ρ 1 argmax-shape))

(include "test/test-E-argmax.rkt")

(provide argmax-1 d-argmax argmax-ρ)
