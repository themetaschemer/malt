#lang racket

(require string-interpolation)
(require "../../accelerated-tensors/ext-impl.rkt")
(require (only-in "../tensors.rkt" ext1-ρ))
(require "../autodiff.rkt")

(define argmax-1-ρ
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    (vset! v-out i-out
      (for/fold ([max -inf.0]
                 [max-i -1] #:result max-i)
          ([i (in-range i0 (+ i0 stride0))])
        (let ((v (vref v0 i)))
          (cond
            ((> v max) (values v (+ (- i i0) 0.0)))
            (else (values max max-i))))))))

(define argmax-1-ρ-acc
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    #<<EOF
    float max = -INFINITY;
    float max_i = -1;
    for(int i=@{i0}; i<@{i0}+@{stride0}; i++) {
        float v = @{v0}[i];
        if(v > max) {
            max = v;
            max_i = i - @{i0} + 0.0;
        }
    }
    @{v-out}[@{i-out}] = max_i;
EOF
    ))

(define argmax-1-∇
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    (let ((z (vref vz iz)))
      (for ([i (in-range i0 (+ i0 stride0))])
        (vset! g0 i 0.0)))))

(define argmax-1-∇-acc
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    #<<EOF
    for(int i=@{i0}; i<@{i0}+@{stride0}; i++) {
        @{g0}[@{i0}] = 0.0;
    }
EOF
))

(define argmax-shape
  (λ (st)
    '()))

(define argmax-1
  (prim1 argmax-1-ρ argmax-1-ρ-acc argmax-1-∇ argmax-1-∇-acc argmax-shape #t))

(define d-argmax
  (ext1 argmax-1 1))

(define argmax-ρ
  (ext1-ρ argmax-1-ρ argmax-1-ρ-acc 1 argmax-shape #t))

(include "test/test-E-argmax.rkt")

(provide argmax-1 d-argmax argmax-ρ)
