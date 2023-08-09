#lang racket

(require "../tensors.rkt")
(require (only-in "../tensors/0-lazy.rkt" force/eval))
(require "A-autodiff.ss")

(require rackunit)

(define forced-ρ
  (λ (d)
    (force/eval (ρ d))))

(define-binary-check (check-dual-equal? equal-wt? actual expected))
(define-check (ρ-∇-checker fn args ans grads)
  (let* ((y (force/eval (apply fn args)))
         (g (force/eval (apply (∇¹ fn) args)))
         (ans-ρ (ρ ans)))
    (cond
      ((and (equal-wt? ans-ρ (ρ y))
            (equal-wt? grads (ρ g))) (void))
      ((equal-wt? ans-ρ (ρ y))
       (fail-check (format "Gradients failed to match.~%actual:~%~s~%expected:~s~%"
                           (ρ g) grads)))
      (else
       (fail-check (format "Answers failed to match.~%actual:~%~s~%expected:~s~%"
                           (ρ y) ans-ρ))))))

(define-syntax check-ρ-∇
  (syntax-rules ()
    [(check-both (fn args ...) ans grads)
     (ρ-∇-checker fn (list args ...) ans grads)]))

(define equal-wt?
  (λ (a b)
    (cond
      ((and (tensor? a) (tensor? b))
       (tensor-equal? a b))
      ((dual? a) (equal-wt? (ρ a) b))
      ((dual? b) (equal-wt? a (ρ b)))
      ((and (vector? a) (vector? b)
            (= (vector-length a) (vector-length b)))
       (vector-andmap equal-wt? a b))
      ((and (pair? a) (pair? b)
            (= (length a) (length b)))
       (andmap equal-wt? a b))
      (else (equal? a b)))))


(define vector-andmap
  (λ (f v1 v2)
    (for/fold ([s #t]) ([v1 v1][v2 v2])
      (and s (f v1 v2)))))

(provide check-dual-equal? check-ρ-∇)
