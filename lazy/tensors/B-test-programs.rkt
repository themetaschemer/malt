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
(struct eval-res-1 (res) #:transparent)
(struct eval-res-2 (res1 res2) #:transparent)


;; Care must be taken while calling get-test-program within the
;; test-program-data thunk because it might lead to an infinite loop.
(define test-programs
  (hasheqv
   'tensor-r1-0 (test-program-data
                 (λ ()
                   (tensor 1 2 3))
                 (eval-res-1 (flat:tensor 1 2 3)))
   'tensor-r1-1 (test-program-data
                 (λ ()
                   (tensor 1 2 3 4 5))
                 (eval-res-1 (flat:tensor 1 2 3 4 5)))
   'tensor-r1-2 (test-program-data
                 (λ ()
                   (tensor 3.0 4.0 5.0))
                 (eval-res-1 (flat:tensor 3.0 4.0 5.0)))
   'tensor-r2-0 (test-program-data
                 (λ ()
                   (tensor (tensor 1 2 3) (tensor 4 5 6)))
                 (eval-res-1 (flat:tensor (flat:tensor 1 2 3) (flat:tensor 4 5 6))))
   'tensor-r2-1 (test-program-data
                 (λ ()
                   (reshape '(2 3) (tensor 3.0 4.0 5.0 7.0 8.0 9.0)))
                 (eval-res-1
                  (flat:reshape '(2 3) (flat:tensor 3.0 4.0 5.0 7.0 8.0 9.0))))
   'build-tensor-r1-0 (test-program-data
                       (λ ()
                         (build-tensor '(6)
                                       (λ (i) (* 3.0 (car i)))))
                       (eval-res-1 (flat:build-tensor '(6)
                                                      (λ (i) (* 3.0 (car i))))))
   'build-tensor-r2-0 (test-program-data
                       (λ ()
                         (build-tensor '(5 6)
                                       (λ (i)
                                         (match-define `(,x ,y) i)
                                         (* 2.0 (+ (* x 6) y)))))
                       (eval-res-1 (flat:build-tensor '(5 6)
                                                      (λ (i)
                                                        (match-define `(,x ,y) i)
                                                        (* 2.0 (+ (* x 6) y))))))
   'build-tensor-r2-1 (test-program-data
                       (λ ()
                         (build-tensor '(3 6)
                                       (λ (i)
                                         (match-define `(,x ,y) i)
                                         (* 3.0 (+ (* x 6) y)))))
                       (eval-res-1 (flat:build-tensor '(3 6)
                                                      (λ (i)
                                                        (match-define `(,x ,y) i)
                                                        (* 3.0 (+ (* x 6) y))))))
   'build-tensor-r3-0 (test-program-data
                       (λ ()
                         (build-tensor '(2 3 4)
                                       (λ (i)
                                         (match-define `(,x ,y ,z) i)
                                         (* 2 (+ (* x 12) (* y 4) (* 1 z))))))
                       (eval-res-1 (flat:build-tensor
                                    '(2 3 4)
                                    (λ (i)
                                      (match-define `(,x ,y ,z) i)
                                      (* 2 (+ (* x 12) (* y 4) (* 1 z)))))))
   'build-tensor-r3-1 (test-program-data
                       (λ ()
                         (build-tensor '(3 5 6)
                                       (λ (i)
                                         (match-define `(,x ,y ,z) i)
                                         (* 2.0 (+ (* x 30) (* y 6) (* 1 z))))))
                       (eval-res-1 (flat:build-tensor
                                    '(3 5 6)
                                    (λ (i)
                                      (match-define `(,x ,y ,z) i)
                                      (* 2.0 (+ (* x 30) (* y 6) (* 1 z)))))))
   'extract-ds-once-tref (test-program-data
                          (λ ()
                            (let ((n (tref (get-test-program 'tensor-r1-0) 1)))
                              (+-ρ n n)))
                          (eval-res-1 4))
   'extract-ds-once-trefs (test-program-data
                           (λ ()
                             (let ((tp (trefs (get-test-program 'tensor-r1-0)
                                              '(0 2))))
                               (+-ρ tp tp)))
                           (eval-res-1 (flat:tensor 2 6)))
   'built-tensor (test-program-data
                  (λ ()
                    (let ((test-build-shape '(4 3)))
                      (build-tensor test-build-shape
                                    (λ (i)
                                      (let ([row (car i)]
                                            [column (cadr i)])
                                        (+ (* (sub1 (car test-build-shape))
                                              row)
                                           column))))))
                  (eval-res-1 (flat:tensor (flat:tensor 0 1 2)
                                           (flat:tensor 3 4 5)
                                           (flat:tensor 6 7 8)
                                           (flat:tensor 9 10 11))))
   'multi-built-tensor (test-program-data
                        (λ ()
                          (+-ρ (get-test-program 'build-tensor-r2-0)
                               (tref (get-test-program 'build-tensor-r3-1) 0)))
                        (eval-res-1 ((flat:ext2-ρ * 0 0) 2
                                                         (flat:build-tensor
                                                          '(5 6)
                                                          (λ (i)
                                                            (match-define `(,x ,y) i)
                                                            (* 2.0 (+ (* x 6) y)))))))
   'tcomp-tref (test-program-data
                (λ ()
                  (make-tref-test-program (get-test-program 'tensor-r1-0)))
                (eval-res-1 3))
   'tcomp-tref-nested (test-program-data
                       (λ ()
                         (tref (tref (get-test-program 'tensor-r2-0) 0) 2))
                       (eval-res-1 3))
   'tcomp-list->tensor (test-program-data
                        (λ ()
                          (make-list->tensor-test-program '(5 6 7 8)))
                        (eval-res-1 (flat:tensor 5 6 7 8)))
   'tcomp-nested-list->tensor (test-program-data
                               (λ ()
                                 (list->tensor
                                  `(,(get-test-program 'tensor-r1-0)
                                    ,(get-test-program 'tensor-r1-0)
                                    ,(get-test-program 'tensor-r1-0))))
                               (eval-res-1 (flat:tensor
                                            (flat:tensor 1 2 3)
                                            (flat:tensor 1 2 3)
                                            (flat:tensor 1 2 3))))
   'tcomp-trefs (test-program-data
                 (λ ()
                   (trefs (get-test-program 'built-tensor) '(0 2)))
                 (eval-res-1 (flat:tensor (flat:tensor 0 1 2)
                                          (flat:tensor 6 7 8))))
   'tcomp-reshape (test-program-data
                   (λ ()
                     (reshape '(3 2 1)
                              (trefs (get-test-program 'built-tensor) '(1 3))))
                   (eval-res-1 (flat:tensor (flat:tensor (flat:tensor 3)
                                                         (flat:tensor 4))
                                            (flat:tensor (flat:tensor 5)
                                                         (flat:tensor 9))
                                            (flat:tensor (flat:tensor 10)
                                                         (flat:tensor 11)))))
   'sum (test-program-data
         (λ ()
           (sum (get-test-program 'tensor-r2-0)))
         (eval-res-1 (flat:tensor 6.0 15.0)))
   'sum-nested (test-program-data
                (λ ()
                  (tensor 4.0 (sum (tensor 1 2 3)) 5.0))
                (eval-res-1 (flat:tensor 4.0 6.0 5.0)))
   'id (test-program-data
        (λ ()
          (id-ρ (get-test-program 'tensor-r2-0)))
        (eval-res-1 (flat:tensor (flat:tensor 1 2 3)
                                 (flat:tensor 4 5 6))))
   'id-scalar (test-program-data
               (λ ()
                 (id-ρ (sum (tensor 4 5 6))))
               (eval-res-1 15))
   'abs-scalar (test-program-data
                (λ ()
                  (abs-ρ (tref (tensor 4 -5 6) 1)))
                (eval-res-1 5))
   'sqr (test-program-data
         (λ ()
           (*-ρ (get-test-program 'build-tensor-r3-0)
                (get-test-program 'build-tensor-r3-0)))
         (eval-res-1 (flat:reshape
                      '(2 3 4)
                      (flat:tensor
                       0  4 16 36
                       64 100 144 196
                       256 324 400 484
                       576 676 784 900
                       1024 1156 1296 1444
                       1600 1764 1936 2116))))
   'r-1-2 (test-program-data
           (λ ()
             (*-2-1 (get-test-program 'build-tensor-r2-0)
                    (get-test-program 'build-tensor-r1-0)))
           (eval-res-1 (flat:reshape
                        '(5 6)
                        (flat:tensor
                         0 6.0 24.0 54.0 96.0 150.0
                         0 42.0 96.0 162.0 240.0 330.0
                         0 78.0 168.0 270.0 384.0 510.0
                         0 114.0 240.0 378.0 528.0 690.0
                         0 150.0 312.0 486.0 672.0 870.0))))
   'r-3-4 (test-program-data
           (λ ()
             (*-2-1 (get-test-program 'build-tensor-r3-1)
                    (get-test-program 'build-tensor-r2-1)))
           (eval-res-1 (flat:reshape
                        '(3 5 6)
                        (flat:tensor
                         0         6.0   24.0   54.0   96.0  150.0
                         0        42.0   96.0  162.0  240.0  330.0
                         0        78.0  168.0  270.0  384.0  510.0
                         0       114.0  240.0  378.0  528.0  690.0
                         0       150.0  312.0  486.0  672.0  870.0

                         1080.0 1302.0 1536.0 1782.0 2040.0 2310.0
                         1296.0 1554.0 1824.0 2106.0 2400.0 2706.0
                         1512.0 1806.0 2112.0 2430.0 2760.0 3102.0
                         1728.0 2058.0 2400.0 2754.0 3120.0 3498.0
                         1944.0 2310.0 2688.0 3078.0 3480.0 3894.0

                         4320.0 4758.0 5208.0 5670.0 6144.0 6630.0
                         4752.0 5226.0 5712.0 6210.0 6720.0 7242.0
                         5184.0 5694.0 6216.0 6750.0 7296.0 7854.0
                         5616.0 6162.0 6720.0 7290.0 7872.0 8466.0
                         6048.0 6630.0 7224.0 7830.0 8448.0 9078.0))))
   'r-sum-2-scalar (test-program-data
                    (λ ()
                      (*-ρ (sum (get-test-program 'build-tensor-r1-0))
                           (sum (tensor 2 3 4))))
                    (eval-res-1 405.0))
   'tcomp-dsqr-r1 (test-program-data
                   (λ ()
                     (d-sqr r1-td (one-like r1-td)))
                   (eval-res-1 (flat:tensor 6.0 8.0 10.0)))
   'gsqr (test-program-data
          (λ ()
            (let ([r2-td (get-test-program 'tensor-r2-1)])
              (d-sqr r2-td (one-like r2-td))))
          (eval-res-1 (flat:reshape
                       '(2 3)
                       (flat:tensor 6.0 8.0 10.0 14.0 16.0 18.0))))
   'g+ (test-program-data
        (λ ()
          (d+ 2.0 3.0 1.0))
        (eval-res-2 1.0 1.0))
   'g-twice (test-program-data
             (λ ()
               (d+ r1-td r1-td (one-like r1-td)))
             (eval-res-2 (flat:tensor 1.0 1.0 1.0)
                         (flat:tensor 1.0 1.0 1.0)))
   'g+-r1-r2 (test-program-data
              (λ ()
                (let ((r2-td (get-test-program 'tensor-r2-1)))
                  (d+ r1-td r2-td (one-like r2-td))))
              (eval-res-2 (flat:tensor 2.0 2.0 2.0)
                          (flat:reshape
                           '(2 3)
                           (flat:tensor 1.0 1.0 1.0 1.0 1.0 1.0))))
   'g* (test-program-data
        (λ ()
          (*∇ (tensor 2.0 3.0 4.0)
              (tensor 1.0 2.0 3.0)
              (tensor 1.0 1.0 1.0)))
        (eval-res-2 (flat:tensor 1.0 2.0 3.0)
                    (flat:tensor 2.0 3.0 4.0)))
   'gsum-r1 (test-program-data
             (λ ()
               (sum-∇ (tensor 2.0 3.0 4.0)
                      1.0))
             (eval-res-1 (flat:tensor 1.0 1.0 1.0)))
   'gsum-r2 (test-program-data
             (λ ()
               (sum-∇ (tensor (tensor 2.0 3.0 4.0)
                              (tensor 2.0 3.0 4.0))
                      (tensor 2.0 1.0)))
             (eval-res-1 (flat:tensor (flat:tensor 2.0 2.0 2.0)
                                      (flat:tensor 1.0 1.0 1.0))))
   'gs2-r1 (test-program-data
            (λ ()
              (s2-∇ (tensor 2.0 3.0 4.0)
                    (tensor 1.0 2.0 3.0)
                    (tensor 1.0 1.0)))
            (eval-res-2 (flat:tensor 1.0 1.0 1.0)
                        (flat:tensor 1.0 1.0 1.0)))
   'gs2-r3 (test-program-data
            (λ ()
              (s2-∇ (tensor (tensor (tensor 1.0 2.0 6.0)
                                    (tensor 3.0 4.0 6.0))
                            (tensor (tensor 5.0 6.0 6.0)
                                    (tensor 7.0 8.0 6.0))
                            (tensor (tensor 8.0 7.0 6.0)
                                    (tensor 6.0 5.0 6.0)))
                    (tensor (tensor (tensor 6.0 8.0 6.0)
                                    (tensor 3.0 4.0 6.0))
                            (tensor (tensor 9.0 7.0 6.0)
                                    (tensor 8.0 2.0 6.0))
                            (tensor (tensor 9.0 7.0 6.0)
                                    (tensor 5.0 1.0 6.0)))
                    (tensor (tensor (tensor 1.0 1.0)
                                    (tensor 1.0 1.0))
                            (tensor (tensor 1.0 1.0)
                                    (tensor 1.0 1.0))
                            (tensor (tensor 1.0 1.0)
                                    (tensor 1.0 1.0)))))
            (eval-res-2 (flat:reshape '(3 2 3)
                                      (flat:list->tensor (make-list 18 1.0)))
                        (flat:reshape '(3 2 3)
                                      (flat:list->tensor (make-list 18 1.0)))))
   'env-flat-scalar (test-program-data
                     (λ ()
                       ((λ (theta) (*-ρ (list-ref theta 0) (list-ref theta 1)))
                        (list (tensor 1.0) 3.0)))
                     (eval-res-1 (flat:tensor 3.0)))
   'common-subexpression (test-program-data
                          (λ ()
                            (let ((t (tref (tensor 1 2 3) 0)))
                              (tensor t t)))
                          (eval-res-1 (flat:tensor 1.0 1.0)))
   'nested-common-subexpression (test-program-data
                                 (λ ()
                                   (let ((t1 (tref (tensor (tensor 1 2 3)
                                                           (tensor 4 5 6))
                                                   0)))
                                     (let ((t0 (tref t1 0)))
                                       (tensor t0 t0))))
                                 (eval-res-1 (flat:tensor 1.0 1.0)))
   ))

(define get-test-program
  (λ (name)
    ((test-program-data-prog-thunk (hash-ref test-programs name)))))
(define get-test-eval-res
  (λ (name)
    (test-program-data-eval-res (hash-ref test-programs name))))

(define sum-f
  (λ (in-v iᵢ sᵢ out-v iₒ sₒ)
    (vset! out-v iₒ
            (for/fold ([sum 0.0]) ([i (in-range iᵢ (+ iᵢ sᵢ))])
              (+ sum (vref in-v i))))))

(define sum (ext1-ρ sum-f 1 (λ (s) '()) #t))

(define id-f (lambda (v) v))
(define id-ρ (ext1-ρ id-f 0 (λ (s) s)))

(define *-ρ (ext2-ρ * 0 0))

(define *-2-1-f
  (λ (v0 i0 s0 v1 i1 s1 vout iout sout)
    (for ([j0 (in-range 0 s0)])
      (vset! vout (+ iout j0)
              (* (vref v0 (+ i0 j0))
                (vref v1 (+ i1 (modulo j0 s1))))))))

(define t2
  (get-test-program 'build-tensor-r1-0))

(define *-2-1
  (ext2-ρ *-2-1-f 2 1 (λ (s0 s1) s0) #t))

(define r1-td (get-test-program 'tensor-r1-2))

(define +ᶠ +)
(define +ᵈ (λ (a b z) (values z z)))

(define sqrᶠ (λ (a) (* a a)))
(define sqrᵈ
  (λ (a z) (* z 2 a)))

(define d-sqr (ext1-∇ sqrᵈ 0))

(define one-like
  (λ (t)
    (build-tensor (shape t) (λ (_) 1.0))))

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
