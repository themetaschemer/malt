#lang racket

(require (only-in "../tensors.rkt" ext1-ρ tref tmap))
(require (rename-in "A-scalar-ops.ss" (+-ρ +)))
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")

(define sum-1-ρ
  (λ (t)
    (summed-ρ t (sub1 (tlen t)) 0.0)))

(define summed-ρ
  (λ (t i a)
    (let ((next-a (+ a (tref t i))))
      (cond
        ((zero? i) next-a)
        (else (summed-ρ t (sub1 i) next-a))))))

(define sum-1-∇
  (λ (t z)
    (tmap (λ (t) z) t)))

(provide sum-1-ρ sum-1-∇)

(define sum-1
  (prim1 sum-1-ρ sum-1-∇))

(define d-sum
  (ext1 sum-1 1))

(define sum-ρ
  (ext1-ρ sum-1-ρ 1))

(define d-sum-cols
  (ext1 sum-1 2))

(define sum-cols-ρ
  (ext1-ρ sum-1-ρ 2))

(include "test/test-D-sum.rkt")

(provide sum-1 d-sum sum-ρ d-sum-cols sum-cols-ρ)
