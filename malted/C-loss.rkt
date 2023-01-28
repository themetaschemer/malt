#lang racket

(require "../base.rkt")
(require "A-core.ss")

(define l2-loss
  (λ (target)
    (λ (xs ys)
      (λ (theta)
        (let ((pred-ys ((target xs) theta)))
          (sum
            (sqr
              (- ys pred-ys))))))))

(define cross-entropy-loss
  (λ (target)
    (λ (xs ys)
      (λ (theta)
        (let ((pred-ys ((target xs) theta))
              (num-classes (ref (reverse (shape ys)) 0)))
           (* -1
             (/ (dot-product ys (log pred-ys))
                num-classes)))))))

(define kl-loss
  (λ (target)
    (λ (xs ys)
      (λ (theta)
        (let ((pred-ys ((target xs) theta)))
          (sum (* pred-ys (log (/ pred-ys ys)))))))))

(define with-recording
  (λ (loss-fn)
    (λ (target)
      (λ (xs ys)
        (λ (theta)
          (record 'loss
            (((loss-fn target) xs ys) theta)))))))

(include "test/test-C-loss.rkt")

(provide l2-loss cross-entropy-loss kl-loss with-recording)
