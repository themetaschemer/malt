#lang racket

;;—————————————————–
;; Lists
;;—————————————————–

(define len length)
(define ref list-ref)
(define refr drop)

(provide len ref refr)

;;—————————————————–
;; Representation of tensors as
;; nested vectors of numbers
;;—————————————————–

(define tref vector-ref)
(define tlen vector-length)
(define tmap vector-map)
(define list->tensor list->vector)
(define tensor vector)
(define scalar? number?)
(define tensor?
  (λ (t)
    (or (scalar? t)
        (vector? t))))

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
         tensor tensor? build-tensor trefs)
