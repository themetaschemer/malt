#lang racket

(require (only-in "../tensors.rkt" ext2 build-tensor ref tref tlen))
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")

(define correlate-3-2
  (λ (bank signal)
    (build-tensor (list (tlen signal) (tlen bank))
      (λ (idx)
        (let ((segment (ref idx 0))
              (filter (tref bank (ref idx 1))))
          (correlate-overlap filter signal segment))))))

(define correlate-overlap
  (λ (filter signal segment)
    (let ((q (/ (- (tlen filter) 1) 2)))
      (let ((from (- segment q))
            (to (+ segment q)))
        (sum-dp filter signal from (sub1 (tlen filter)) 0.0)))))

(define sum-dp
  (λ (filter signal from i a)
    (let ((si (+ from i)))
      (let ((a-hat
             (cond
               ((and (>= si 0) (< si (tlen signal)))
                (+-0-0 a
                   (dot-product (tref signal si)
                                (tref filter i))))
               (else a))))
        (cond
          ((zero? i) a-hat)
          (else (sum-dp filter signal from (sub1 i) a-hat)))))))

(define dot-product
  (λ (t u)
    (dotted-product t u (sub1 (tlen t)) 0.0)))

(define dotted-product
  (λ (t u i a)
    (let ((a-hat (+-0-0 a (*-0-0 (tref t i) (tref u i)))))
      (cond
        ((zero? i) a-hat)
        (else (dotted-product t u (sub1 i) a-hat))))))

(define d-correlate
  (ext2 correlate-3-2 3 2))

(include "test/test-G-correlate.rkt")

(provide correlate-3-2 d-correlate)
