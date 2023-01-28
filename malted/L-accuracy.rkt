#lang racket

(require "../base.rkt")

(define model
  (λ (target theta)
    (λ (t)
      ((target t) theta))))

(define accuracy
  (λ (a-model xs ys)
    (ρ (/ (sum (=-1 (argmax (a-model xs)) (argmax ys)))
          (tlen ys)))))

(include "test/test-L-accuracy.rkt")
(provide accuracy model)
