#lang racket
(require "0-lazy.rkt")
(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))

(define make-tref-test-program
  (λ (t)
    (tref t 2)))
(define make-list->tensor-test-program
  (λ (l)
    (list->tensor l)))

(struct test-program-data (prog-thunk eval-res) #:transparent)
(define test-programs
  (hasheqv
   'tensor-r1-0 (test-program-data
                 (λ ()
                   (tensor 1 2 3))
                 (flat:tensor 1 2 3))
   'tensor-r1-1 (test-program-data
                 (λ ()
                   (tensor 1 2 3 4 5))
                 (flat:tensor 1 2 3 4 5))
   'tensor-r2-0 (test-program-data
                 (λ ()
                   (tensor (tensor 1 2 3) (tensor 4 5 6)))
                 (flat:tensor (flat:tensor 1 2 3) (flat:tensor 4 5 6)))
   'build-tensor-r2-0 (test-program-data
                       (λ ()
                         (build-tensor '(5 6)
                                       (λ (i)
                                         (match-define `(,x ,y) i)
                                         (* 2.0 (+ (* x 6) y)))))
                       (flat:build-tensor '(5 6)
                                          (λ (i)
                                            (match-define `(,x ,y) i)
                                            (* 2.0 (+ (* x 6) y)))))
   'build-tensor-r3-0 (test-program-data
                       (λ ()
                         (build-tensor '(2 3 4)
                                       (λ (i)
                                         (match-define `(,x ,y ,z) i)
                                         (* 2 (+ (* x 12) (* y 4) (* 1 z))))))
                       (flat:build-tensor '(2 3 4)
                                          (λ (i)
                                            (match-define `(,x ,y ,z) i)
                                            (* 2 (+ (* x 12) (* y 4) (* 1 z))))))
   'build-tensor-r3-1 (test-program-data
                       (λ ()
                         (build-tensor '(3 5 6)
                                       (λ (i)
                                         (match-define `(,x ,y ,z) i)
                                         (* 2.0 (+ (* x 30) (* y 6) (* 1 z))))))
                       (flat:build-tensor '(3 5 6)
                                          (λ (i)
                                            (match-define `(,x ,y ,z) i)
                                            (* 2.0 (+ (* x 30) (* y 6) (* 1 z))))))
   'extract-ds-once-tref (test-program-data
                          (λ ()
                            (let ((n (tref (get-test-program 'tensor-r1-0) 1)))
                              (+-ρ n n)))
                          4)
   'extract-ds-once-trefs (test-program-data
                           (λ ()
                             (let ((tp (trefs (get-test-program 'tensor-r1-0) '(0 2))))
                               (+-ρ tp tp)))
                           (flat:tensor 2 6))
   'built-tensor (test-program-data
                  (λ ()
                    (build-tensor test-build-shape
                                  (λ (i)
                                    (let ([row (car i)]
                                          [column (cadr i)])
                                      (+ (* (sub1 (car test-build-shape))
                                            row)
                                         column)))))
                  (flat:tensor (flat:tensor 0 1 2)
                               (flat:tensor 3 4 5)
                               (flat:tensor 6 7 8)
                               (flat:tensor 9 10 11)))
   'multi-built-tensor (test-program-data
                        (λ ()
                          (+-ρ (get-test-program 'build-tensor-r2-0)
                               (tref (get-test-program 'build-tensor-r3-1) 0)))
                        ((flat:ext2-ρ * 0 0) 2 (flat:build-tensor '(5 6)
                                                                  (λ (i)
                                                                    (match-define `(,x ,y) i)
                                                                    (* 2.0 (+ (* x 6) y))))))
   ))

(define get-test-program
  (λ (name)
    ((test-program-data-prog-thunk (hash-ref test-programs name)))))
(define get-test-eval-res
  (λ (name)
    (test-program-data-eval-res (hash-ref test-programs name))))

(define test-tcomp-tref (make-tref-test-program (get-test-program 'tensor-r1-0)))
(define test-tcomp-tref-nested (tref (tref (get-test-program 'tensor-r2-0) 0) 2))
(define test-list->tensor (make-list->tensor-test-program '(5 6 7 8)))
(define test-nested-list->tensor
  (list->tensor `(,(get-test-program 'tensor-r1-0)
                  ,(get-test-program 'tensor-r1-0)
                  ,(get-test-program 'tensor-r1-0))))
(define test-build-shape '(4 3))

(define test-refs '(0 2))
(define test-trefs (trefs (get-test-program 'built-tensor) test-refs))
(define test-reshape (reshape '(3 2 1) (trefs (get-test-program 'built-tensor) '(1 3))))

(define sum-f
  (λ (in-v iᵢ sᵢ out-v iₒ sₒ)
    (vset! out-v iₒ
            (for/fold ([sum 0.0]) ([i (in-range iᵢ (+ iᵢ sᵢ))])
              (+ sum (vref in-v i))))))

(define sum (ext1-ρ sum-f 1 (λ (s) '()) #t))
(define test-tp-sum (sum (get-test-program 'tensor-r2-0)))
(define test-tp-sum-nested (tensor 4.0 (sum (tensor 1 2 3)) 5.0))

(define id-f (lambda (v) v))
(define id-ρ (ext1-ρ id-f 1 (λ (s) s)))
(define test-tp-id (id-ρ (get-test-program 'tensor-r2-0)))
(define test-tp-id-scalar (id-ρ (sum (tensor 4 5 6))))

(define t0
  (get-test-program 'build-tensor-r3-0))

(define *-ρ (ext2-ρ * 0 0))
(define t0sqr (*-ρ t0 t0))

(define *-2-1-f
  (λ (v0 i0 s0 v1 i1 s1 vout iout sout)
    (for ([j0 (in-range 0 s0)])
      (vset! vout (+ iout j0)
              (* (vref v0 (+ i0 j0))
                (vref v1 (+ i1 (modulo j0 s1))))))))

(define t1
  (get-test-program 'build-tensor-r2-0))

(define t2
  (build-tensor '(6)
                  (λ (i) (* 3.0 (car i)))))

(define *-2-1
  (ext2-ρ *-2-1-f 2 1 (λ (s0 s1) s0) #t))

(define r-1-2
  (*-2-1 t1 t2))

(define t3
  (get-test-program 'build-tensor-r3-1))

(define t4
  (build-tensor '(3 6)
                  (λ (i)
                    (match-define `(,x ,y) i)
                    (* 3.0 (+ (* x 6) y)))))

(define r-3-4
  (*-2-1 t3 t4))

(define r-sum-2-scalar (*-ρ (sum t2) (sum (tensor 2 3 4))))

(define r1-td (tensor 3.0 4.0 5.0))
(define r2-td (reshape '(2 3) (tensor 3.0 4.0 5.0 7.0 8.0 9.0)))

(define +ᶠ +)
(define +ᵈ (λ (a b z) (values z z)))

(define sqrᶠ (λ (a) (* a a)))
(define sqrᵈ
  (λ (a z) (* z 2 a)))

(define d-sqr (ext1-∇ sqrᵈ 0))

(define one-like
  (λ (t)
    (build-tensor (shape t) (λ (_) 1.0))))

(define tcomp-dsqr-r1 (d-sqr r1-td (one-like r1-td)))

(define d+ (ext2-∇ +ᵈ 0 0))

(define *∇ (ext2-∇ (λ (a b z) (values (* z b) (* z a))) 0 0))

(define sum-1-∇
  (λ (g t it st vz iz sz)
    (for* ([i (in-range it (+ it st))])
      (vset! g i (vref vz iz)))))

(define sum-∇ (ext1-∇ sum-1-∇ 1 (λ (s) '()) #t))

;; t and u must have the same shape
(define s2-f (lambda (t u) (tensor (sum t) (sum u))))
(define s2-d
  (λ (g0 g1 t it st u iu su vz iz sz)
    (for* ([i (in-range it (+ it st))])
      (vset! g0 i (vref vz iz))
      (vset! g1 i (vref vz (+ iz 1))))))
(define s2-∇ (ext2-∇ s2-d 1 1 (λ (s0 s1) (list 2)) #t))

(define test-env-flat-scalar
  ((λ (theta) (*-ρ (list-ref theta 0) (list-ref theta 1)))
    (list (tensor 1.0) 3.0)))

;; Check common subexpression introduced by let is not repeated
(define test-common-subexpr
  (let ((t (tref (tensor 1 2 3) 0)))
    (tensor t t)))

(define test-common-nested-subexprs
  (let ((t1 (tref (tensor (tensor 1 2 3) (tensor 4 5 6)) 0)))
    (let ((t0 (tref t1 0)))
      (tensor t0 t0))))

(define random-tensor
  (λ (s)
    (build-tensor s (λ (tidx) (random 10)))))
(define test-build-random
  (let ((v (random-tensor '(3 2 4))))
    (*-ρ v v)))

(define +-ρ (ext2-ρ + 0 0))
(define /-ρ (ext2-ρ / 0 0))
(define --ρ (ext2-ρ - 0 0))
(define abs-ρ (ext1-ρ abs 0))

(define mean
  (λ (t)
    (abs-ρ (/-ρ (sum (sum t)) (size-of (shape t))))))
(define variance
  (λ (t)
    (--ρ (/-ρ (sum (sum (*-ρ t t))) (size-of (shape t)))
         (*-ρ (mean t) (mean t)))))

(define v (random-tensor '(10 4)))
(define mean-v (mean v))
(define variance-v (variance v))

(define r (random-tensor '(10 4 2)))
(define mean-r (mean r))
(define variance-r (variance r))

(define -ᶠ -)
(define -ᵈ (λ (a b z) (values z (- z))))
(define d- (ext2-∇ -ᵈ 0 0))


(provide (all-defined-out))
