#lang racket

(require (only-in "../tensors.rkt" ext1-ρ))
(require "../autodiff.rkt")

(define max-1-ρ
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    (vector-set! v-out i-out
      (for/fold ([max 0.0])
          ([i (in-range i0 (+ i0 stride0))])
        (let ((v (vector-ref v0 i)))
          (cond
            ((> v max) v)
            (else max)))))))

(define max-1-∇
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    (let ((z (vector-ref vz iz)))
      (for/fold ([max -inf.0]
                 [max-i -1] #:result
                 (for ([i (in-range i0 (+ i0 stride0))])
                   (cond
                     ((= i (+ i0 max-i)) (vector-set! g0 i z))
                     (else (vector-set! g0 i 0.0)))))
                ([i (in-range i0 (+ i0 stride0))])
        (let ((v (vector-ref v0 i)))
          (cond
            ((> v max) (values v (- i i0)))
            (else (values max max-i))))))))

(define max-shape
  (λ (st)
    (cdr st)))

(define max-1
  (prim1 max-1-ρ max-1-∇ max-shape))

(define d-max
  (ext1 max-1 1))

(define max-ρ
  (ext1-ρ max-1-ρ 1 max-shape))

(include "test/test-F-max.rkt")

(provide max-1 d-max max-ρ)
