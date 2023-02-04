#lang racket

(require "../base.rkt")
(require "A-core.ss")

(define line
  (λ (xs)
    (λ (theta)
      (+ (* (ref theta 0) xs) (ref theta 1)))))

(define quad
  (λ (x)
    (λ (theta)
      (let ((a (ref theta 0))
            (b (ref theta 1))
            (c (ref theta 2)))
        (+ (* a (sqr x))
           (+ (* b x)
              c))))))

(define linear-1-1
  (λ (t)
    (λ (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

(define linear
  (λ (t)
    (λ (theta)
      (+ (dot-product-2-1 (ref theta 0) t) (ref theta 1)))))

(define plane
  (λ (t)
    (λ (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

(define softmax
  (λ (t)
    (λ (theta)
      (let ((z (- t (max t))))
        (let ((expz (exp z)))
          (/ expz (sum expz)))))))

(define avg-cols
  (λ (t)
    (λ (theta)
      (let ((num-cols (ref (refr (shape t) (- (rank t) 2)) 0)))
        (/ (sum-cols t) num-cols)))))

(define signal-avg
  (λ (t)
    (λ (theta)
      (let ((num-segments (ref (refr (shape t) (- (rank t) 2)) 0)))
        (/ (sum-cols t) num-segments)))))

(include "test/test-B-targets.rkt")

(provide line quad linear-1-1 linear plane softmax avg-cols signal-avg)
