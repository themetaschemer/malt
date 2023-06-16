#lang racket

(require (only-in "../tensors.rkt" ext2-ρ tref tlen))
(require (only-in "../autodiff.rkt" prim2 ext2))

(define concat-1-1-ρ
  (λ (t u)
    (let ((nt (tlen t))
          (nu (tlen u)))
      (build-vector (+ nt nu)
        (λ (i)
          (cond
            ((< i nt) (tref t i))
            (else (tref u (- i nt)))))))))

(define concat-1-1-∇
  (λ (t u z)
    (let ((nt (tlen t))
          (nu (tlen u)))
      (values
       (build-vector nt
         (λ (i)
           (tref z i)))
       (build-vector nu
         (λ (i)
           (tref z (+ i nt))))))))

(define concat-1-1
  (prim2 concat-1-1-ρ concat-1-1-∇))

(define d-concat
  (ext2 concat-1-1 1 1))

(define concat-ρ
  (ext2-ρ concat-1-1-ρ 1 1))

(include "test/test-K-concat.rkt")

(provide concat-1-1 d-concat concat-ρ)
