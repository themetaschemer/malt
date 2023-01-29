#lang racket

(require "B-tensor-basics.ss")

;;—————————————————–
;; Shape, rank, size-of
;;—————————————————–
(define shape
  (λ (t)
    (cond
     ((scalar? t) '())
     (else (cons (tlen t) (shape (tref t 0)))))))

(define rank
  (λ (t)
    (len (shape t))))

(define size-of
  (λ (s)
    (sized s 1)))

(define sized
  (λ (s a)
    (cond
      ((null? s) a)
      (else (sized (refr s 1) (* a (ref s 0)))))))

;;—————————————————–
;; Reshape a tensor
;;—————————————————–

(define reshape
  (λ (s t)
    (cond
      ((= (size-of s) (size-of (shape t)))
       (reshape-tensor s t))
      (else (error 'reshape
                   "Cannot reshape ~a into ~a~%"
                   (shape t) s)))))

(define reshape-tensor
  (λ (s t)
    (let ((t-strides (strides (shape t)))
          (s-strides (strides s)))
      (build-tensor s
        (λ (idx)
          (let ((t-idx (convert-idx t-strides s-strides idx)))
            (deep-tref t t-idx)))))))

;; convert an idx into a tensor of shape s
;; into an idx into a tensor of shape st
(define convert-idx
  (λ (t-strides s-strides idx)
    (invert-reference t-strides
      (flat-ref s-strides idx))))

(define flat-ref
  (λ (strides idx)
    (flat-reference strides idx 0)))

(define flat-reference
  (λ (strides idx a)
    (cond
      ((null? strides) a)
      (else (flat-reference (refr strides 1) (refr idx 1)
               (+ a (* (ref strides 0) (ref idx 0))))))))

(define strides
  (λ (s)
    (cond
      ((null? s) '())
      (else (cons (size-of (refr s 1))
                  (strides (refr s 1)))))))

(define invert-reference
  (λ (stride idx)
    (cond
      ((null? stride) '())
      (else
       (cons (quotient idx (ref stride 0))
         (invert-reference (refr stride 1)
           (remainder idx (ref stride 0))))))))

(define deep-tref
  (λ (t idx)
    (cond
      ((null? idx) t)
      (else
       (deep-tref (tref t (ref idx 0))
         (refr idx 1))))))


(include "test/test-C-tensor-ops.rkt")

(provide rank shape reshape size-of)
