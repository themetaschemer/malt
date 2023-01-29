#lang racket

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
      ((dual? d) (vector-ref d 1))
      (else d))))

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

;;----------------------------
;; Chain rule
;;----------------------------

(define end-of-chain
  (λ (d z σ)
    (let ((g (hash-ref σ d 0.0)))
      (hash-set σ d (+ z g)))))

(provide dual? dual dual* ρ κ scalar? end-of-chain)
