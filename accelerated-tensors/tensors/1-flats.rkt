#lang racket

;--------------------------------------------------------
; Representation of tensors
;--------------------------------------------------------

;; A flat tensor representation is for a contiguous slice in the backing store.
;; The fields we need:
;;   shape : list
;;   store : vector
;;   offset : start of the contiguous slice.
;;   size : number of elements in the contiguous slice
;;   strides : Number of elements in each dimension of the tensor.
;;   rank: Number of dimensions in the tensor



(define flat
  (λ (shape store offset)
    (vector flat shape store offset
            (size-of shape)
            (strides shape)
            (length shape))))

(define flat?
  (λ (v)
    (and (vector? v)
         (eq? (vector-ref v 0) flat))))

(define flat-shape
  (λ (f)
    (vector-ref f 1)))

(define flat-store
  (λ (f)
    (vector-ref f 2)))

(define flat-offset
  (λ (f)
    (vector-ref f 3)))

(define flat-size
  (λ (f)
    (vector-ref f 4)))

(define flat-strides
  (λ (f)
    (vector-ref f 5)))

(define flat-rank
  (λ (f)
    (vector-ref f 6)))

(define size-of
  (λ (shape)
    (product shape 1)))

(define product
  (λ (lst a)
    (cond
      ((null? lst) a)
      (else (product (cdr lst) (* (car lst) a))))))

(define strides
  (λ (shape)
    (cond
      ((null? shape) '())
      (else (cons (size-of (cdr shape))
                  (strides (cdr shape)))))))

(provide flat flat? flat-shape flat-store
         flat-offset flat-rank flat-strides flat-size
         size-of strides)
