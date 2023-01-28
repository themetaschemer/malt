#lang racket

(require "../tensors.rkt")
(require "A-autodiff.rkt")
(require rackunit)

(define-binary-check (check-dual-equal? equal-wt? actual expected))

(define-check (ρ-∇-checker fn args ans grads)
  (let* ((y (apply fn args))
         (g (apply (∇¹ fn) args)))
    (cond
      ((and (equal-wt? ans (ρ y))
            (equal-wt? grads (ρ g))) (void))
      ((equal-wt? ans (ρ y))
       (fail-check (format "Gradients failed to match.~%actual:~%~s~%expected:~s~%"
                           (ρ g) grads)))
      (else
       (fail-check (format "Answers failed to match.~%actual:~%~s~%expected:~s~%"
                           (ρ y) ans))))))

(define-syntax check-ρ-∇
  (syntax-rules ()
    [(check-both (fn args ...) ans grads)
     (ρ-∇-checker fn (list args ...) ans grads)]))

(define equal-wt?
  (λ (a b)
    (cond
      ((dual? a) (equal-wt? (ρ a) b))
      ((dual? b) (equal-wt? a (ρ b)))
      ((and (tensor? a) (tensor? b)) (tensor-equal? a b))
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
