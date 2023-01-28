#lang racket

(require (only-in "../tensors.rkt" ext2-ρ build-tensor ref tref))
(require (except-in "../autodiff.rkt" ref))

(require "A-scalar-ops.ss")

;;—————————————————–
;; Correlation
;; In the following
;; Bank has rank 3, shape (b m d)
;; Signal has rank 2, shape (n d)
;; filter has rank 2, shape (m d)
;; segment has rank 1, shape (d)
;; point has rank 1, shape (d)
;;
;; Result of correlation is a Signal
;;
;; This code is highly dependent upon
;; the tensor representation
;; (it uses build-vector)
;;—————————————————–

(define correlate-3-2-ρ
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
                (+ a
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
    (let ((a-hat (+ a (* (tref t i) (tref u i)))))
      (cond
        ((zero? i) a-hat)
        (else (dotted-product t u (sub1 i) a-hat))))))

;; z is a signal of shape (n b)
(define correlate-3-2-∇
  (λ (bank signal z)
    (values (derivative-of-bank bank signal z)
      (derivative-of-signal bank signal z))))

;; returns a shape (b m d)
(define derivative-of-bank
  (λ (bank signal z)
    (build-vector (tlen bank)
      (λ (zi)
        (sum-over-filter (tref bank zi) signal z zi)))))

;; returns a shape (m d)
(define sum-over-filter
  (λ (filter signal z zi)
    (let ((q (/ (- (tlen filter) 1) 2)))
      (for/vector ([fi (in-range 0 (tlen filter))])
        (sum-over-segments (tref filter fi) signal z zi (- fi q))))))

;; returns a shape (d)
(define sum-over-segments
  (λ (point signal z zi qi)
    (summed-over-segments point signal z zi qi (sub1 (tlen signal)) 0.0)))

(define summed-over-segments
  (λ (point signal z zi qi i a)
    (let ((si (+ i qi)))
      (let ((a-hat
             (cond
               ((and (>= si 0) (< si (tlen signal)))
                (+-ρ a (*-ρ (tref (tref z i) zi) (tref signal si))))
               (else a))))
        (cond
          ((zero? i) a-hat)
          (else  (summed-over-segments point signal z zi qi(sub1 i) a-hat)))))))

;; returns a shape (n d)
(define derivative-of-signal
  (λ (bank signal z)
    (let ((n (tlen signal))
          (s (shape bank)))
      (let ((m (ref s 1)))
        (let ((q (/ (- m 1) 2)))
          (build-vector n
            (λ (si)
              (sum-over-bank bank (tref signal si) (tref z si) si n m q))))))))

;; returns a shape d
(define sum-over-bank
  (λ (bank segment z si n m q)
    (summed-over-bank bank segment z si n m q (sub1 (tlen bank)) 0.0)))

(define summed-over-bank
  (λ (bank segment z si n m q i a)
    (let ((filter (tref bank i))
          (zi     (tref z i)))
      (let ((a-hat (summed-over-filter filter segment zi (- si q) n m (sub1 m) a)))
        (cond
          ((zero? i) a-hat)
          (else (summed-over-bank bank segment z si n m q (sub1 i) a-hat)))))))

;; returns a shape d
(define summed-over-filter
  (λ (filter segment this-z siq n m i a)
    (let ((signal-index (+ i siq))
          (filter-index (- (- m 1) i)))
      (let ((a-hat
             (cond
               ((and (>= signal-index 0)
                     (< signal-index n))
                (+-ρ a (*-ρ (tref filter filter-index) this-z)))
               (else a))))
        (cond
          ((zero? i) a-hat)
          (else (summed-over-filter filter segment this-z siq n m (sub1 i) a-hat)))))))

(define correlate-3-2
  (prim2 correlate-3-2-ρ correlate-3-2-∇))

(define d-correlate
  (ext2 correlate-3-2 3 2))

(define correlate-ρ
  (ext2-ρ correlate-3-2-ρ 3 2))

(include "test/test-G-correlate.rkt")

(provide correlate-ρ d-correlate)
