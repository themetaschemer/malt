#lang racket

(require string-interpolation)
(require (only-in "../tensors.rkt" ext1-ρ ext2-ρ))
(require "../autodiff.rkt")

(define +-0-0-ρ-acc
  (λ (a b)
    "@{a}+@{b}"))

(define +-0-0
  (prim2 +
         +-0-0-ρ-acc
         (λ (a b z)
           (values z z))
         (λ (a b z)
           (values z z))))

(define --0-0-ρ-acc
  (λ (a b)
    "@{a}-@{b}"))

(define --0-0
  (prim2 -
         --0-0-ρ-acc
         (λ (a b z)
           (values z (- z)))
         (λ (a b z)
           (values z "(- @{z})"))))

(define *-0-0-ρ-acc
  (λ (a b)
    "@{a}*@{b}"))

(define *-0-0
  (prim2 *
         *-0-0-ρ-acc
         (λ (a b z)
           (values (* b z) (* a z)))
         (λ (a b z)
           (values "@{b}*@{z}" "@{a}*@{z}"))))

(define /-0-0-ρ-acc
  (λ (a b)
    "@{a}/@{b}"))

(define /-0-0
  (prim2 /
         /-0-0-ρ-acc
         (λ (a b z)
           (values (* z (/ 1 b))
                   (* z (/ (- a) (* b b)))))
         (λ (a b z)
           (values "(@{z} * (1 / @{b}))"
                   "(@{z} * ((- @{a}) / (@{b} * @{b})))"))))

(define expt-0-0-ρ-acc
  (λ (a b)
    "pow(@{a}, @{b})"))

(define expt-0-0
  (prim2 expt
         expt-0-0-ρ-acc
         (λ (a b z)
           (values (* z (* b (expt a (- b 1))))
                   (* z (* (expt a b) (log a)))))
         (λ (a b z)
           (values "(@{z} * (@{b} * pow(@{a}, (@{b} - 1))))"
                   "(@{z} * (pow(@{a}, @{b}) * log(@{a})))"))))

(define exp-0-ρ-acc
  (λ (a)
    "exp(@{a})"))

(define exp-0
  (prim1 exp
         exp-0-ρ-acc
         (λ (a z)
           (* z (exp a)))
         (λ (a z)
           "(@{z} * exp(@{a}))")))

(define log-0-ρ-acc
  (λ (a)
    "log(@{a})"))

(define log-0
  (prim1 log
         log-0-ρ-acc
         (λ (a z)
           (* z (/ 1 a)))
         (λ (a z)
           "(@{z} * (1 / @{a}))")))

(define sqrt-0-ρ-acc
  (λ (a)
    "sqrt(@{a})"))

(define sqrt-0
  (prim1 sqrt
         sqrt-0-ρ-acc
         (λ (x z)
           (/ z (* 2 (sqrt x))))
         (λ (x z)
           "(@{z} / (2 * sqrt(@{x})))")))

(define abs-0-ρ
  (λ (x)
    (cond
      ((< x 0) (* -1 x))
      (else x))))

(define abs-0-ρ-acc
  (λ (x)
    "fabs(@{x})"))

(define abs-0-∇
  (λ (x z)
    (cond
     ((< x 0) (- z))
     (else z))))

(define abs-0-∇-acc
  (λ (x z)
    "sign(@{x}) * @{z}"))

(define abs-0
  (prim1 abs-0-ρ abs-0-ρ-acc abs-0-∇ abs-0-∇-acc))

(define rectify-0-ρ
  (λ (s)
    (cond
      ((< s 0.0) 0.0)
      (else s))))

(define rectify-0-ρ-acc
  (λ (s)
    "fmax(0.0f, @{s})"))

(define rectify-0-∇
  (λ (s z)
    (cond
      ((< s 0.0) 0.0)
      (else z))))

(define rectify-0-∇-acc
  (λ (s z)
    "step(0, @{s}) * @{z}"))

(define rectify-shape
  (λ (s) s))

(define rectify-0
  (prim1 rectify-0-ρ rectify-0-ρ-acc rectify-0-∇ rectify-0-∇-acc rectify-shape))

;;------------------------------------
;; differentiable extended functions.
;;------------------------------------

(define d* (ext2 *-0-0 0 0))
(define d+ (ext2 +-0-0 0 0))
(define d- (ext2 --0-0 0 0))
(define d/ (ext2 /-0-0  0 0))
(define d-expt (ext2 expt-0-0 0 0))

(define d-exp (ext1 exp-0 0))
(define d-log (ext1 log-0 0))
(define d-abs (ext1 abs-0 0))
(define d-rectify (ext1 rectify-0 0))
(define d-sqrt (ext1 sqrt-0 0))

(define d-sqr
  (λ (x)
    (d* x x)))

;;------------------------------------
;; non-differentiable extended functions.
;;------------------------------------

(define *-ρ (ext2-ρ * *-0-0-ρ-acc 0 0))
(define +-ρ (ext2-ρ + +-0-0-ρ-acc 0 0))
(define --ρ (ext2-ρ - --0-0-ρ-acc 0 0))
(define /-ρ (ext2-ρ / /-0-0-ρ-acc 0 0))
(define expt-ρ (ext2-ρ expt expt-0-0-ρ-acc 0 0))

(define exp-ρ (ext1-ρ exp exp-0-ρ-acc 0))
(define log-ρ (ext1-ρ log log-0-ρ-acc 0))
(define abs-ρ (ext1-ρ abs-0-ρ abs-0-ρ-acc 0))
(define rectify-ρ (ext1-ρ rectify-0-ρ rectify-0-ρ-acc 0))
(define sqrt-ρ (ext1-ρ sqrt sqrt-0-ρ-acc 0))

(define sqr-ρ
  (λ (x)
    (*-ρ x x)))

(include "test/test-A-scalar-ops.rkt")

(provide +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
         exp-0 log-0 abs-0 rectify-0 sqrt-0

         d+ d- d* d/
         d-expt d-exp d-log d-abs
         d-rectify d-sqrt d-sqr

         +-ρ --ρ *-ρ /-ρ
         expt-ρ exp-ρ log-ρ abs-ρ
         rectify-ρ sqrt-ρ sqr-ρ)