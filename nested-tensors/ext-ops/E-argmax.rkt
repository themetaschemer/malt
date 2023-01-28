#lang racket

(require (only-in "../tensors.rkt" ext1-ρ tref tmap))
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")

(define argmax-1-ρ
  (λ (t)
    (let ((i (sub1 (tlen t))))
      (argmaxed-ρ t i i))))

(define argmaxed-ρ
  (λ (t i a)
    (let ((a-hat (next-argmax t i a)))
      (cond
        ((zero? i) a-hat)
        (else (argmaxed-ρ t (sub1 i) a-hat))))))

(define next-argmax
  (λ (t i a)
    (cond
      ((> (tref t i) (tref t a)) i)
      (else a))))

(define argmax-1-∇
  (λ (t z)
    (tmap (λ (x) 0.0) t)))

(define argmax-1
  (prim1 argmax-1-ρ argmax-1-∇))

(define d-argmax
  (ext1 argmax-1 1))

(define argmax-ρ
  (ext1-ρ argmax-1-ρ 1))

(include "test/test-E-argmax.rkt")

(provide argmax-1 d-argmax argmax-ρ)
