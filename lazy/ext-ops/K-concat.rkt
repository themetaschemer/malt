#lang racket

(require string-interpolation)
(require "../../accelerated-tensors/ext-impl.rkt")
(require (rename-in (only-in "../tensors.rkt" ext2-ρ tref tlen shape len ref)
                    (shape shape-ρ)))
(require (only-in "../autodiff.rkt" prim2 ext2 shape))

(define concat-shape
  (λ (st su)
    (cons (+ (ref st 0) (ref su 0))
          (cdr st))))

(define concat-base-ρ
  (λ (v0 i0 stride0
      v1 i1 stride1
      v-out i-out stride-out)
    (for ([i (in-range 0 stride-out)])
      (cond
        ((< i stride0)
         (vset! v-out (+ i-out i) (vref v0 (+ i0 i))))
        (else
         (vset! v-out (+ i-out i) (vref v1 (+ i1 (- i stride0)))))))))

(define concat-base-ρ-acc
  (λ (v0 i0 stride0
      v1 i1 stride1
      v-out i-out stride-out)
  #<<EOF
    for(int i=0; i < @{stride-out}; i++) {
        if (i < @{stride0}) {
            @{v-out}[i+@{i-out}] = @{v0}[i+@{i0}];
        } else {
            @{v-out}[i+@{i-out}] = @{v1}[(i-@{stride0})+@{i1}];
        }
    }
EOF
))

(define concat-base-∇
  (λ (g0 g1 v0 i0 stride0
      v1 i1 stride1
      vz iz stride-z)
    (for ([i (in-range 0 stride-z)])
      (cond
        ((< i stride0)
         (vset! g0 (+ i0 i)
           (+ (vref g0 (+ i0 i))
              (vref vz (+ iz i)))))
        (else
         (vset! g1 (+ i1 (- i stride0))
           (+ (vref g1 (+ i1 (- i stride0)))
              (vref vz (+ iz i)))))))))

(define concat-base-∇-acc
  (λ (g v0 i0 stride0
      v1 i1 stride1
      vz iz stride-z)
    (values
   #<<EOF
    for(int i=0; i < @{stride0}; i++) {
        @{g}[i+@{i0}] += @{vz}[i+@{iz}];
    }
EOF

   #<<EOF
    for(int i=@{stride0}; i < @{stride-z}; i++) {
        @{g}[i-@{stride0}+@{i1}] += @{vz}[i+@{iz}];
    }
EOF
   )))

(define concat-base
  (prim2 concat-base-ρ concat-base-ρ-acc concat-base-∇ concat-base-∇-acc concat-shape #t))

(define d-concat-n
  (λ (n)
    (λ (t u)
      (let ((st (shape t))
            (su (shape u)))
        (ensure-compatible-shapes n st su)
        ((ext2 concat-base n n) t u)))))

(define concat-n-ρ
  (λ (n)
    (λ (t u)
      (let ((st (shape-ρ t))
            (su (shape-ρ u)))
        (ensure-compatible-shapes n st su)
        ((ext2-ρ concat-base-ρ concat-base-ρ-acc n n concat-shape #t) t u)))))

(define ensure-compatible-shapes
  (λ (n st su)
    (let ((rt (len st))
          (ru (len su)))
      ;; The shape of the tensor of rank r at rank n-1
      ;; is given by (drop st (+ 1 (- r n)))
      (when (not (equal? (drop st (+ 1 (- rt n))) (drop su (+ 1 (- ru n)))))
        (error 'concat "Incompatible concat shapes: ~a and ~a at last ~a dimensions"
          st su n)))))

(define d-concat (d-concat-n 1))
(define concat-ρ (concat-n-ρ 1))
(define concat-1-1 concat-base)

(include "test/test-K-concat.rkt")

(provide concat-1-1
         d-concat concat-ρ
         d-concat-n concat-n-ρ)
