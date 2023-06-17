#lang racket

(require (rename-in (only-in "../tensors.rkt" ext2-ρ tref tlen shape len)
                    (shape shape-ρ)))
(require (only-in "../autodiff.rkt" prim2 ext2 shape))

(define concat-base-ρ
  (λ (t u)
    (let ((nt (tlen t))
          (nu (tlen u)))
      (build-vector (+ nt nu)
        (λ (i)
          (cond
            ((< i nt) (tref t i))
            (else (tref u (- i nt)))))))))

(define concat-base-∇
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

(define concat-base
  (prim2 concat-base-ρ concat-base-∇))

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
        ((ext2-ρ concat-base-ρ n n) t u)))))

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

(provide concat-1-1 d-concat concat-ρ d-concat-n concat-n-ρ)
