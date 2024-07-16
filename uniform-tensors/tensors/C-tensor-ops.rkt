#lang racket

(require "1-flats.ss")
(require "B-tensor-basics.ss")

;;—————————————————–
;; Shape, rank, size-of
;;—————————————————–

(define shape
  (λ (t)
    (cond
      ((number? t) '())
      (else (flat-shape t)))))

(define rank
  (λ (t)
    (len (shape t))))

;;—————————————————–
;; Reshape a tensor
;;—————————————————–

(define reshape
  (λ (s t)
    (cond
      ((= (size-of s) (flat-size t))
       (flat s (flat-store t) (flat-offset t)))
      (else (error 'tensor-reshape "Cannot reshape ~a to ~a~%" (flat-shape t) s)))))


(include "test/test-C-tensor-ops.rkt")

(provide rank shape reshape)
(provide size-of strides)
