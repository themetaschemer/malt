#lang racket

(require "../tensors.rkt")

;;----------------------------
;; Reverse-mode AD
;;----------------------------

(define ∇
  (λ (f theta)
    (let ((wrt (map* dual* theta)))
      (∇-once (f wrt) wrt))))

(define ∇¹
  (λ (f)
    (λ xs
      (let ((wrt (map* dual* xs)))
        (∇-once (apply f wrt) wrt)))))

(define ∇-once
  (λ (y wrt)
    (let ((σ (∇σ y (hasheq))))
      (map* (λ (d)
              (hash-ref σ d 0.0))
        wrt))))

(define ∇σ
  (λ (y σ)
    (cond
      ((scalar? y)
       ((κ y) y 1.0 σ))
      ((list? y)
       (∇σ-list y σ))
      ((vector? y)
       (∇σ-vector y (sub1 (tlen y)) σ))
      (else (printf "Unknown: ~a~%" y)))))

(define ∇σ-list
  (λ (y σ)
    (cond
      ((null? y) σ)
      (else
       (let ((σ-hat (∇σ (ref y 0) σ)))
         (∇σ-list (refr y 1) σ-hat))))))

(define ∇σ-vector
  (λ (y i σ)
    (let ((σ-hat (∇σ (tref y i) σ)))
      (cond
        ((zero? i) σ-hat)
        (else (∇σ-vector y (sub1 i) σ-hat))))))

;;----------------------------
;; General helpers
;;----------------------------

(define map*
  (λ (f y)
    (cond
      ((scalar? y) (f y))
      ((list? y)
       (map (λ (lm)
              (map* f lm))
         y))
      ((vector? y)
       (vector-map (λ (ve)
                     (map* f ve))
         y))
      (else
       (error 'map* "Cannot map* ~a" y)))))

(define trace-print
  (λ (v port)
    (cond
      ((dual? v) (trace-print (ρ v) port))
      (else (fprintf port "~a~%" v)))))

(include "test/test-A-autodiff.rkt")

(provide
  dual dual? ρ κ ∇ ∇¹ ∇-once map* dual* scalar?
  trace-print)
