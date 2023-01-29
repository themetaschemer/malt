#lang racket

(require (only-in "../tensors.rkt" ext1 tref tlen))
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")
(require (only-in (rename-in "B-comparators.ss" (>-0-0 >)) >))

(define max-1
  (λ (t)
    (let ((i (sub1 (tlen t))))
      (maxed t i (tref t i)))))

(define maxed
  (λ (t i a)
    (let ((a-hat (next-max t i a)))
      (cond
        ((zero? i) a-hat)
        (else (maxed t (sub1 i) a-hat))))))

(define next-max
  (λ (t i a)
    (cond
      ((> (tref t i) a) (tref t i))
      (else a))))

(define d-max
  (ext1 max-1 1))

(include "test/test-F-max.rkt")

(provide max-1 d-max)
