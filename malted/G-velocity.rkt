#lang racket

;; Extended operators are non-dualized
(require "../base-no-duals.rkt")
(require "D-gradient-descent.ss")
(require "E-gd-common.ss")


;; Velocity/Momentum

(define velocity-i
  (λ (p)
    (list p (zeroes p))))

(define velocity-d
  (λ (pa)
    (ref pa 0)))

(define velocity-u
  (λ (pa g)
    (let ((v (- (* mu (ref pa 1)) (* alpha g))))
      (list (+ (ref pa 0) v) v))))

(define velocity-gradient-descent
  (gradient-descent
    velocity-i velocity-d velocity-u))

(include "test/test-G-velocity.rkt")

(provide velocity-gradient-descent)
