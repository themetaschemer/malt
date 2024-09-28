#lang racket

(require string-interpolation)
(require (only-in "../tensors.rkt" ext1-ρ tref reshape shape ref))
(require (only-in "../autodiff.rkt" prim1 ext1))

(define flatten-2-ρ
  (λ (t)
    (reshape (flatten-shape (shape t)) t)))

(define flatten-2-ρ-acc
  (λ (v0 i0 stride0
      v-out i-out stride-out)
    #<<EOF
    for(int i=@{i0}; i<@{i0}+@{stride0}; i++) {
        @{v-out}[i] = @{v0}[i];
    }
EOF
    ))

(define flatten-2-∇
  (λ (t z)
    (reshape (shape t) z)))

(define flatten-2-∇-acc
  (λ (g0 v0 i0 stride0
      vz iz stride-z)
    #<<EOF
    for(int i=@{i0}; i<@{i0}+@{stride0}; i++) {
        @{g0}[i] += @{vz}[i];
    }
EOF
    ))

(define flatten-shape
  (λ (s)
    (let ((rows (ref s 0))
          (cols (ref s 1)))
      (list (* rows cols)))))

(define flatten-2
  (prim1 flatten-2-ρ flatten-2-ρ-acc flatten-2-∇ flatten-2-∇-acc flatten-shape))

(define d-flatten
  (ext1 flatten-2 2))

(define flatten-ρ
  (ext1-ρ flatten-2-ρ flatten-2-ρ-acc 2))

(include "test/test-I-flatten.rkt")

(provide flatten-2 d-flatten flatten-ρ)
