#lang racket

(require "../tensors.rkt")
(require "../autodiff.rkt")

;;----------------------------
;; Boolean comparators
;;----------------------------

(define comparator
  (λ (f)
    (λ (da db)
      (f (ρ da) (ρ db)))))

(define =-0-0
  (comparator =))

(define <-0-0
  (comparator <))

(define <=-0-0
  (comparator <=))

(define >-0-0
  (comparator >))

(define >=-0-0
  (comparator >))

;;----------------------------
;; Tensorized comparators
;;----------------------------

(define comparator-ρ
  (λ (f)
    (λ (da db)
      (cond
        ((f (ρ da) (ρ db)) 1.0)
        (else 0.0)))))

(define extended-comparator
  (λ (f)
    (ext2 (comparator-ρ f) 0 0)))

(define =-1
  (extended-comparator =))

(define <-1
  (extended-comparator <))

(define >-1
  (extended-comparator >))

(define <=-1
  (extended-comparator <=))

(define >=-1
  (extended-comparator >=))

(define !=
  (λ (a b)
    (not (= a b))))

(define !=-1
  (extended-comparator !=))

(include "test/test-B-comparators.rkt")

(provide =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
         =-1 <-1 >-1 <=-1 >=-1 !=-1)
