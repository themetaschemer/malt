#lang racket

(require (only-in "../impl.rkt" ρ))

;;---------------------------------------
;; The Normal Distribution
;;---------------------------------------

(define random-normal
  (λ (mu sigma)
    (+ (ρ mu) (* (ρ sigma) (random-standard-normal)))))

(define random-standard-normal
  (λ ()
    (one-normal-random-number
     (- (* 2 (random)) 1.0)
     (random num-parts))))

(provide random-normal random-standard-normal)

;;---------------------------------------
;; Ziggurat algorithm for converting a
;; uniform random number to a normal distribution.
;;---------------------------------------

;; Number partitions in the
(define num-parts 128)

;; FIXME: (Maybe) Compute the two following  using a formula. It is probably
;; based on num-part. Issue: std-deviation is turning out to be
;; a little off (~0.04), probably because 128 may be too few parts.

;; Start of right tail
(define start-of-tail 3.442619855899)

;; Area of partitions
(define area-of-partitions 9.91256303526217e-3)

(define init-boxes
  (λ (num-boxes tail area)
    (list->vector
     (list-of-boxes num-boxes tail area
       (exp (* -0.5 start-of-tail start-of-tail))))))

(define list-of-boxes
  (λ (num-boxes tail area f)
    `(,(/ area f) ,tail
      ,@(more-boxes num-boxes area 2 f tail '()))))

(define more-boxes
  (λ (n area i f last a)
    (cond
      ((= i n) (reverse (cons 0 a)))
      (else
       (let ((this-sqr (log (+ (/ area last) f))))
         (let ((this (sqrt (* -2 this-sqr))))
           (more-boxes n area (add1 i) (exp this-sqr)
             this (cons this a))))))))

(define boxes
  (init-boxes num-parts
     start-of-tail area-of-partitions))

(define one-normal-random-number
  (λ (u i)
    (cond
     ((zero? i) (normal-tail start-of-tail (< u 0)))
     (else (non-base-ziggurat-case u i)))))

(define non-base-ziggurat-case
  (λ (u i)
    (let* ((b (vector-ref boxes i))
           (c (vector-ref boxes (add1 i)))
           (z (* u b)))
      (cond
       ((in-the-box? z c) z)
       ((in-bleed-area? b c z) z)
       (else (random-standard-normal))))))

(define in-the-box?
  (λ (z c)
    (< (abs z) c)))

(define in-bleed-area?
  (λ (b c z)
    (let* ((fb (phi b))
           (fc (phi c))
           (fz (phi (abs z)))
           (box-height (- fc fb))
           (over-height (- fz fb))
           (u1 (random)))
      (< (* u1 box-height)
         over-height))))

;; Function to calculate tail of the base box.
(define normal-tail
  (λ (min negative?)
    (let ((x (/ (log (random)) min))
          (y (log (random))))
      (cond
        ((< (* -2 y) (* x x))
         (normal-tail min negative?))
        (negative? (- x min))
        (else (- min x))))))

;; Standard normal distribution. Mean 0, std 1,
(define phi
  (λ (x)
    (/ (exp (* -0.5 (sqr x))) (sqrt (* 2 pi)))))

(include "test/test-B-random.rkt")
