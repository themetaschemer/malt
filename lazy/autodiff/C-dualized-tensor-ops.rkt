#lang racket

(require "../tensors.rkt")
(require "A-autodiff.ss")


;;----------------------------
;; Tensor ops, cleaned up.
;;----------------------------

(define d-rank
  (lambda (t)
    (rank (ρ t))))

(define d-shape
  (λ (t)
    (shape (ρ t))))

(define d-reshape
  (λ (s t)
    (cond
      ((dual? t)
       (dual (reshape s (ρ t))
             (κ t)))
      (else (reshape s t)))))

(define d-trefs
  (λ (t b)
    (trefs (ρ t) b)))

(define d-tensor?
  (λ (t)
    (tensor? (ρ t))))

(define d-tlen
  (λ (t)
    (tlen (ρ t))))

(define d-ref
  (λ (l i)
    (ref l (ρ i))))

(define d-refr
  (λ (l i)
    (refr l (ρ i))))

(provide d-rank d-shape d-reshape d-trefs d-tensor? d-tlen d-ref d-refr)
