#lang racket

;; Extended operators are non-dualized
(require "../base-no-duals.rkt")
(require "D-gradient-descent.ss")
(require "E-gd-common.ss")

(define adam-i
  (λ (p)
    (let ((zeroed (zeroes p)))
      (list p zeroed zeroed))))

(define adam-d
  (λ (pa)
    (ref pa 0)))

(define adam-u
  (λ (pa g)
    (let ((r (smooth beta (ref pa 2) (sqr g))))
      (let ((alpha-hat (/ alpha (+ (sqrt r) epsilon)))
            (v (smooth mu (ref pa 1) g)))
        (list (- (ref pa 0) (* alpha-hat v)) v  r)))))

(define adam-gradient-descent
  (gradient-descent
    adam-i adam-d adam-u))

(include "test/test-I-adam.rkt")

(provide adam-gradient-descent)
