#lang racket

(require "../tensors.rkt")
(require "../tensors/c0-ast.rkt")
(require "A-autodiff.ss")
(require (except-in "../../accelerated-tensors/ext-impl.rkt"
                    scalarize))

(require rackunit)

(define force-print-store
  (λ (t)
    (with-output-to-string
      (λ ()
        (print-vec (flat-store (↓ t)
                               #;(list-ref (unbox (tpromise-dst t)) 0)))))))

(define-binary-check (check-dual-equal? equal-wt? actual expected))
(define-check (ρ-∇-checker fn args ans grads)
  (let* ((y (apply fn args))
         (g (apply (∇¹ fn) args))
         (ans-ρ (ρ ans)))
    (cond
      ((and (equal-wt? ans-ρ (ρ y))
            (equal-wt? grads (ρ g))) (void))
      ((equal-wt? ans-ρ (ρ y))
       (fail-check (format "Gradients failed to match.~%actual:~%~s~%expected:~%~s~%~%actual store:~%~a~%expected store:~%~a~%"
                           (ρ g) grads (map force-print-store (ρ g)) (map force-print-store grads))))
      (else
       (fail-check (format "Answers failed to match.~%actual:~%~s~%expected:~%~s~%~%actual store:~%~a~%expected store:~%~a~%"
                           (ρ y) ans-ρ (force-print-store (ρ y)) (force-print-store ans-ρ)))))))

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
