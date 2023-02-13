#lang racket

(require "../autodiff.rkt")
(require (only-in "../tensors.rkt" ext1 ext2))

(define +-0-0
  (prim2 +
         (λ (a b z)
           (values z z))))

(define --0-0
  (prim2 -
         (λ (a b z)
           (values z (- z)))))

(define *-0-0
  (prim2 *
         (λ (a b z)
           (values (* b z) (* a z)))))

(define /-0-0
  (prim2 /
    (λ (a b z)
      (values (* z (/ 1 b))
        (* z (/ (- a) (* b b)))))))

(define expt-0-0
  (prim2 expt
    (λ (a b z)
      (values (* z (* b (expt a (- b 1))))
        (* z (* (expt a b) (log a)))))))

(define exp-0
  (prim1 exp
    (λ (a z)
      (* z (exp a)))))

(define log-0
  (prim1 log
    (λ (a z)
      (* z (/ 1 a)))))

(define abs-0
  (prim1 abs
    (λ (x z)
      (cond
        ((< x 0) (- z))
        (else z)))))

(define sqrt-0
  (prim1 sqrt
    (λ (x z)
      (/ z (* 2 (sqrt x))))))

;;------------------------------------
;; Extended functions.
;;------------------------------------

(define d* (ext2 *-0-0 0 0))
(define d+ (ext2 +-0-0 0 0))
(define d- (ext2 --0-0 0 0))
(define d/ (ext2 /-0-0  0 0))
(define d-expt (ext2 expt-0-0 0 0))

(define d-exp (ext1 exp-0 0))
(define d-log (ext1 log-0 0))
(define d-abs (ext1 abs-0 0))

(define d-sqrt (ext1 sqrt-0 0))

(define d-sqr
  (λ (x)
    (d* x x)))

(include "test/test-A-scalar-ops.rkt")

(provide
 +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
 exp-0 log-0 abs-0 sqrt-0

 d+ d- d* d/
 d-expt d-exp d-log d-abs
 d-sqrt d-sqr)
