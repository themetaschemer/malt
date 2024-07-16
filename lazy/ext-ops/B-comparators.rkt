#lang racket

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
  (comparator >=))

;;----------------------------
;; Tensorized comparators
;;----------------------------

(define comparator-ρ
  (λ (f)
    (λ (da db)
      (cond
        ((f (ρ da) (ρ db)) 1.0)
        (else 0.0)))))

(define comparator-∇
  (λ (f)
    (λ (da db z)
      (cond
        ((f (ρ da) (ρ db)) (values z z))
        (else (values 0.0 0.0))))))

(define comparator-shape
  (λ (f)
    (λ (sa sb)
      sa)))

(define comparator-prim
  (λ (f)
    (prim2 (comparator-ρ f) (comparator-∇ f) (comparator-shape f))))

(define extended-comparator
  (λ (f)
    (ext2 (comparator-prim f) 0 0)))

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
