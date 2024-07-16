#lang racket

(require "../tensors.rkt")

;;----------------------------
;; Real part of a dual is always a tensor (of any rank)
;;----------------------------

(define dual?
  (λ (x)
    (and (vector? x) (eq? (vector-ref x 0) dual))))

(define dual
  (λ (r k)
    (vector dual r k)))

(define dual*
  (λ (d)
    (dual (ρ d) end-of-chain)))

(define ρ
  (λ (d)
    (cond
      ((dual? d) (scalarize (vector-ref d 1)))
      (else (scalarize d)))))

(define κ
  (λ (d)
    (cond
      ((dual? d) (vector-ref d 2))
      (else end-of-chain))))

(define scalar?
  (λ (d)
    (or (number? d)
        (and (dual? d)
             (number? (ρ d))))))

(define dual-like?
  (λ (d)
    (or (dual? d)
        (number? d)
        (tensor? d))))

;;----------------------------
;; Chain rule
;;----------------------------

(define end-of-chain
  (λ (d z σ)
    (let ((g (hash-ref σ d 0.0)))
      (hash-set σ d (+-ρ z g)))))

(define +-ρ
  (ext2-ρ + 0 0))

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
              (tp-force (hash-ref σ d 0.0)))
        wrt))))

(define ∇σ
  (λ (y σ)
    (cond
      ((dual-like? y) ((κ y) y (one-like (ρ y)) σ))
      ((list? y) (∇σ-list y σ))
      (else (printf "Unknown: ~a~%" y)))))

(define ∇σ-list
  (λ (y σ)
    (cond
      ((null? y) σ)
      (else
       (let ((σ-hat (∇σ (ref y 0) σ)))
         (∇σ-list (refr y 1) σ-hat))))))

;;----------------------------
;; General helpers
;;----------------------------

(define map*
  (λ (f y)
    (cond
      ((dual-like? y) (f y))
      ((list? y)
       (map (λ (yi)
              (map* f yi))
         y))
      (else y))))

(define trace-print
  (λ (v port)
    (cond
      ((dual? v) (trace-print (ρ v) port))
      (else (fprintf port "~a~%" v)))))

(define (one-like s) ((ext1-ρ (λ (x) 1.0) 0) s))

(include "test/test-A-autodiff.rkt")

(provide
  dual dual? ρ κ ∇ ∇¹ dual* scalar? end-of-chain
  trace-print map*)
