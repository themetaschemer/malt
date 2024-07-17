#lang racket

(require string-interpolation)
(require "../../accelerated-tensors/ext-impl.rkt")
(require (only-in "../tensors.rkt" ext1-ρ))
(require "../autodiff.rkt")

(define sum-1-ρ
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    (vset! v-out i-out
      (for/fold ([sum 0.0]) ([i (in-range i0 (+ i0 stride0))])
        (+ sum (vref v0 i))))))

(define sum-1-ρ-acc
  (λ (v0 i0 stride0
      v-out i-out stride-out)
  #<<EOF
    float sum = 0;
    for (int i=@{i0}; i < @{i0}+@{stride0}; i++) {
        sum += @{v0}[i];
    }
    @{v-out}[@{i-out}] = sum;
EOF
    ))

(define sum-1-∇
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    (let ((z (vref vz iz)))
      (for ([i (in-range i0 (+ i0 stride0))])
        (vset! g0 i
          (+ (vref g0 i) z))))))

(define sum-1-∇-acc
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
  #<<EOF
    float z = @{vz}[@{iz}];
    for (int i=@{i0}; i < @{i0}+@{stride0}; i++) {
        @{g0}[i] += z;
    }
EOF
    ))

(define sum-shape
  (λ (st)
    (refr st 1)))

(define sum-1
  (prim1 sum-1-ρ sum-1-ρ-acc sum-1-∇ sum-1-∇-acc sum-shape #t))

(define d-sum
  (ext1 sum-1 1))

(define sum-ρ
  (ext1-ρ sum-1-ρ sum-1-ρ-acc 1 sum-shape #t))

(provide d-sum sum-ρ)

(define sum-cols-2-ρ
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    (for ((i (in-range 0 stride-out)))
      (vset! v-out (+ i i-out)
        (for/fold ([sum 0.0]) ([j (in-range i0 (+ i0 stride0) stride-out)])
          (+ sum (vref v0 (+ j i))))))))

(define sum-cols-2-ρ-acc
  (λ (v0 i0 stride0 v-out i-out stride-out)
    #<<EOF
    for(int i=0; i<@{stride-out}; i++) {
        float sum = 0.0;
        for(int j=@{i0}; j<@{i0}+@{stride0}; j+=@{stride-out}) {
            sum += @{v0}[j+i];
        }
        @{v-out}[i+@{i-out}] = sum;
    }
EOF
    ))

(define sum-cols-2-∇
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    (for ((i (in-range 0 stride-z)))
      (for ([j (in-range i0 (+ i0 stride0) stride-z)])
        (vset! g0 (+ i j)
          (+ (vref g0 (+ i j)) (vref vz (+ i iz))))))))

(define sum-cols-2-∇-acc
  (λ (g0 v0 i0 stride0
         vz iz stride-z)
    #<<EOF
    for(int i=0; i<@{stride-z}; i++) {
        for(int j=@{i0}; j<@{i0}+@{stride0}; j+=@{stride-z}) {
            @{g0}[i+j] += @{vz}[i+@{iz}];
        }
    }
EOF
    ))

(define sum-cols-shape
  (λ (s)
    (refr s 1)))

(define sum-cols-2
  (prim1 sum-cols-2-ρ sum-cols-2-ρ-acc sum-cols-2-∇ sum-cols-2-∇-acc sum-cols-shape #t))

(define d-sum-cols
  (ext1 sum-cols-2 2))

(define sum-cols-ρ
  (ext1-ρ sum-cols-2-ρ sum-cols-2-ρ-acc 2 sum-cols-shape #t))

(include "test/test-D-sum.rkt")

(provide sum-1 d-sum-cols sum-cols-ρ)
