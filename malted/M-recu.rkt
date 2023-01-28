#lang racket

(require "../base.rkt")

(define corr
  (λ (t)
    (λ (theta)
      (+ (correlate (ref theta 0) t) (ref theta 1)))))

(define recu
  (λ (t)
    (λ (theta)
      (rectify ((corr t) theta)))))

(define k-recu
  (λ (k)
    (λ (t)
      (λ (theta)
        (cond
          ((zero? k) t)
          (else (((k-recu (sub1 k))
                  ((recu t) theta))
                 (refr theta 2))))))))

(include "test/test-M-recu.rkt")

(provide recu corr k-recu)
