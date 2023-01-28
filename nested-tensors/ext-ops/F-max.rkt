#lang racket

(require (only-in "../tensors.rkt" ext1-ρ tref))
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")
(require (only-in "E-argmax.ss" argmax-ρ))

(define max-1-ρ
  (λ (t)
    (let ((i (sub1 (tlen t))))
      (maxed-ρ t i (tref t i)))))

(define maxed-ρ
  (λ (t i a)
    (let ((a-hat (next-max t i a)))
      (cond
        ((zero? i) a-hat)
        (else (maxed-ρ t (sub1 i) a-hat))))))

(define next-max
  (λ (t i a)
    (cond
      ((> (tref t i) a) (tref t i))
      (else a))))

(define max-1-∇
  (λ (t z)
    (let ((m (argmax-ρ t)))
      (build-vector (tlen t)
        (λ (i)
          (cond
            ((= i m) z)
            (else 0.0)))))))

(define max-1
  (prim1 max-1-ρ max-1-∇))

(define d-max
  (ext1 max-1 1))

(define max-ρ
  (ext1-ρ max-1-ρ 1))

(include "test/test-F-max.rkt")

(provide max-1 d-max max-ρ)
