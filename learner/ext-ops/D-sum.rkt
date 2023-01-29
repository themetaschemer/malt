#lang racket

(require (only-in "../tensors.rkt" ext1 tref tlen))
(require (only-in (rename-in "A-scalar-ops.ss" (d+ +)) +))
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")

(define sum-1
  (λ (t)
    (summed t (sub1 (tlen t)) 0.0)))

(define summed
  (λ (t i a)
    (let ((next-a (+ a (tref t i))))
      (cond
        ((zero? i) next-a)
        (else (summed t (sub1 i) next-a))))))

(define d-sum
  (ext1 sum-1 1))

(define d-sum-cols
  (ext1 sum-1 2))

(include "test/test-D-sum.rkt")

(provide sum-1 d-sum d-sum-cols)
