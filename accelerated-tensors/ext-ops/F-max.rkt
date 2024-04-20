#lang racket

(require string-interpolation)
(require "../tensors/0-vectors.rkt")
(require (only-in "../tensors.rkt" ext1-ρ))
(require "../autodiff.rkt")

(define max-1-ρ
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    (vset! v-out i-out
      (for/fold ([max -inf.0])
          ([i (in-range i0 (+ i0 stride0))])
        (let ((v (vref v0 i)))
          (cond
            ((> v max) v)
            (else max)))))))

(define max-1-ρ-acc
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    #<<EOF
    float max = -INFINITY;
    for(int i=@{i0}; i<@{i0}+@{stride0}; i++) {
        max = fmax(max, @{v0}[i]);
    }
    @{v-out}[@{i-out}] = max;
EOF
    ))

(define max-1-∇
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    (let ((z (vref vz iz)))
      (for/fold ([max -inf.0]
                 [max-i -1] #:result
                 (for ([i (in-range i0 (+ i0 stride0))])
                   (when (= i (+ i0 max-i))
                     (vset! g0 i (+ (vref g0 i) z)))))
                ([i (in-range i0 (+ i0 stride0))])
        (let ((v (vref v0 i)))
          (cond
            ((> v max) (values v (- i i0)))
            (else (values max max-i))))))))

(define max-1-∇-acc
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    #<<EOF
    float z = @{vz}[@{iz}];
    float max = -INFINITY;
    int max_i = -1;
    for(int i=@{i0}; i<@{i0}+@{stride0}; i++) {
        float v = @{v0}[i];
        if(v > max) {
            max = v;
            max_i = i - @{i0};
        }
    }
    for(int i=@{i0}; i<@{i0}+@{stride0}; i++) {
        if(i == @{i0}+max_i) {
            @{g0}[i] += z;
        } else {
            @{g0}[i] += 0.0;
        }
    }
EOF
    ))

(define max-shape
  (λ (st)
    (cdr st)))

(define max-1
  (prim1 max-1-ρ max-1-ρ-acc max-1-∇ max-1-∇-acc max-shape))

(define d-max
  (ext1 max-1 1))

(define max-ρ
  (ext1-ρ max-1-ρ max-1-ρ-acc 1 max-shape))

(include "test/test-F-max.rkt")

(provide max-1 d-max max-ρ)
