#lang racket

(require "../base-no-duals.rkt")
(require "B-layer-fns.ss")

(define init-theta
  (λ (theta-shapes)
    (map init-shape theta-shapes)))

(define init-shape
  (λ (s)
    (cond
      ((= (len s) 1) (zero-tensor s))
      ((= (len s) 2)
       (let ((fan-in (ref s 1)))
         (random-tensor 0.0 (/ 2 fan-in) s)))
      ((= (len s) 3)
       (let ((fan-in (* (ref s 1) (ref s 2))))
         (random-tensor 0.0 (/ 2 fan-in) s))))))

(define random-tensor
  (λ (mean variance s)
    (build-tensor s
      (λ (tidx)
        (random-normal mean (sqrt variance))))))

(define zero-tensor
  (λ (s)
    (build-tensor s (λ (tidx) 0.0))))

(include "test/test-O-init.rkt")

(provide init-theta init-shape random-tensor zero-tensor)
