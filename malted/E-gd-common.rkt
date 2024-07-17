#lang racket

;; Extended operators are non-dualized
(require "../base-no-duals.rkt")

(define zeroes
  (ext1-ρ (λ (_) 0.0) (λ (_) "0.0") 0))

(define smooth
  (λ (decay-rate average g)
    (+ (* decay-rate average)
       (* (- 1.0 decay-rate) g))))

(declare-hyper mu)
(declare-hyper beta)
(define epsilon 1.0e-8)

(include "test/test-E-gd-common.rkt")

(provide zeroes smooth epsilon)
