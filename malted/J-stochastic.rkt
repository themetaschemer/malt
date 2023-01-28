#lang racket

(require "../base.rkt")

(declare-hyper batch-size)

(define samples
  (λ (n s)
    (sampled n s (list))))

(define sampled
  (λ (n i a)
    (cond
      ((zero? i) a)
      (else
       (sampled n (sub1 i)
         (cons (random n) a))))))

(define sampling-obj
  (λ (expectant xs ys)
    (let ((n (car (shape xs))))
      (λ (theta)
        (let ((b (samples n batch-size)))
          ((expectant (trefs xs b) (trefs ys b)) theta))))))

(include "test/test-J-stochastic.rkt")

(provide samples sampling-obj)
