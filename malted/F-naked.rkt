#lang racket

;; Extended operators are non-dualized
(require "../base-no-duals.rkt")
(require "D-gradient-descent.ss")

;; Naked
(define naked-i
  (lambda (p)
    p))

(define naked-d
  (lambda (pa)
    pa))

(define naked-u
  (λ (pa g)
    (- pa (* alpha g))))

(define naked-gradient-descent
  (gradient-descent
    naked-i naked-d naked-u))

(include "test/test-F-naked.rkt")

(provide naked-gradient-descent)
