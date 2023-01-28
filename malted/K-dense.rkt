#lang racket

(require "../base.rkt")
(require "A-core.ss")
(require "B-layer-fns.ss")

(define relu
  (λ (t)
    (λ (theta)
      (rectify ((linear t) theta)))))

(define k-relu
  (λ (k)
    (λ (t)
      (λ (theta)
        (cond
          ((zero? k) t)
          (else (((k-relu (sub1 k))
                  ((relu t) theta))
                 (refr theta 2))))))))

(include "test/test-K-dense.rkt")

(provide rectify relu k-relu)
