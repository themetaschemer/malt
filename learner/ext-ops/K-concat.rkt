#lang racket

(require "../tensors.rkt")

(define concat-1-1
  (λ (t u)
    (let ((nt (tlen t))
          (nu (tlen u)))
      (build-vector (+ nt nu)
        (λ (i)
          (cond
            ((< i nt) (tref t i))
            (else (tref u (- i nt)))))))))

(define concat
  (ext2 concat-1-1 1 1))

(include "test/test-K-concat.rkt")

(provide concat-1-1 concat)
