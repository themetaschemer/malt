#lang racket

(require "../tensors.rkt")

(define flatten-2
  (λ (t)
    (let ((s (shape t)))
      (let ((rows (ref s 0))
            (cols (ref s 1)))
        (build-vector (* rows cols)
          (λ (i)
            (tref
              (tref t (quotient i cols))
              (remainder i cols))))))))

(define flatten
  (ext1 flatten-2 2))

(include "test/test-I-flatten.rkt")

(provide flatten-2 flatten)
