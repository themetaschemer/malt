#lang racket

(require (only-in "../tensors.rkt" ext1 tlen tref))
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")
(require (only-in (rename-in "B-comparators.ss" (>-0-0 >)) >))

(define argmax-1
  (λ (t)
    (let ((i (sub1 (tlen t))))
      (argmaxed t i i))))

(define argmaxed
  (λ (t i a)
    (let ((a-hat (next-argmax t i a)))
      (cond
        ((zero? i) a-hat)
        (else (argmaxed t (sub1 i) a-hat))))))

(define next-argmax
  (λ (t i a)
    (cond
      ((> (tref t i) (tref t a)) i)
      (else a))))

(define d-argmax
  (ext1 argmax-1 1))

(include "test/test-E-argmax.rkt")

(provide argmax-1 d-argmax)
