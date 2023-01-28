#lang racket

;; Extended operators are non-dualized
(require "../base-no-duals.rkt")
(require "D-gradient-descent.ss")
(require "E-gd-common.ss")

(define epsilon 10e-8)

(define rms-i
  (λ (p)
    (list p (zeroes p))))

(define rms-d
  (λ (pa)
    (ref pa 0)))

(define rms-u
  (λ (pa g)
    (let ((r (smooth beta (ref pa 1) (sqr g))))
      (let ((alpha-hat (/ alpha (+ (sqrt r) epsilon))))
        (list (- (ref pa 0) (* alpha-hat g)) r)))))

(define rms-gradient-descent
  (gradient-descent
    rms-i rms-d rms-u))

(include "test/test-H-rms.rkt")

(provide rms-gradient-descent)
