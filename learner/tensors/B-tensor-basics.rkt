#lang racket

(require "0-duals.rkt")

;;—————————————————–
;; Lists
;;—————————————————–

(define len length)

(define ref
  (λ (l i)
    (list-ref l (ρ i))))

(define refr
  (λ (l i)
    (drop l (ρ i))))

(provide len ref refr)

;;—————————————————–
;; Representation of tensors as
;; nested vectors of numbers
;;—————————————————–

(define tref
  (λ (t i)
    (vector-ref t (ρ i))))

(define tlen vector-length)

(define tmap vector-map)

(define list->tensor list->vector)

(define tensor?
  (λ (t)
    (cond
      ((scalar? t) #t)
      ((vector? t) #t)
      (else #f))))

(define build-tensor
  (λ (s f)
    (built-tensor f s '())))

(define built-tensor
  (λ (f s idx)
    (cond
      ((= (len s) 1)
       (build-vector (ref s 0)
         (λ (i)
           (f (append idx (list i))))))
      (else
       (build-vector (ref s 0)
         (λ (i)
           (built-tensor f (refr s 1) (append idx (list i)))))))))

(define trefs
  (λ (t b)
    (build-vector (len b)
      (λ (i)
        (tref t (list-ref b i))))))

(include "test/test-B-tensor-basics.rkt")

(provide tref tlen tmap list->tensor scalar?
         tensor? build-tensor trefs)
