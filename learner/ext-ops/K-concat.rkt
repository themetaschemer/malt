#lang racket

(require "../tensors.rkt")

(define concat-base
  (λ (t u)
    (let ((nt (tlen t))
          (nu (tlen u)))
      (build-vector (+ nt nu)
        (λ (i)
          (cond
            ((< i nt) (tref t i))
            (else (tref u (- i nt)))))))))

;; n is the rank at which we need to do the concatenation.
;; the shape of the two tensors at rank n-1 should be identical.

(define concat-n
  (λ (n)
    (λ (t u)
      (let ((st (shape t))
            (su (shape u)))
        (ensure-compatible-shapes n st su)
        ((ext2 concat-base n n) t u)))))

(define concat-1-1 concat-base)

(define concat
  (concat-n 1))

(define ensure-compatible-shapes
  (λ (n st su)
    (let ((rt (len st))
          (ru (len su)))
      ;; The shape of the tensor of rank r at rank n-1
      ;; is given by (drop st (+ 1 (- r n)))
      (when (not (equal? (drop st (+ 1 (- rt n))) (drop su (+ 1 (- ru n)))))
        (error 'concat "Incompatible concat shapes: ~a and ~a at last ~a dimensions"
          st su n)))))

(include "test/test-K-concat.rkt")

(provide concat-1-1 concat concat-n)
