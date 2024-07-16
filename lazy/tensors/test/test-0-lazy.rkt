(module+ test
  (require rackunit)

  (define test-lt (tensor 1 2 3))
  (check-true (flat? (tpromise-tensor test-lt)))
  (check-equal? (flat-store (tp-force test-lt)) (vector 1 2 3))
  (check-true (flat? (tpromise-tensor test-lt)))
  (check-exn exn:fail? (λ () (tensor test-lt 4)))
  (check-exn exn:fail? (λ () (tensor 4 test-lt)))

  (check-equal? (tp-tref test-lt 2) 3)
  (check-exn exn:fail? (λ () (tp-tref test-lt 5)))

  (define test-nested-lt (tensor (tensor 1 2 3) (tensor 4 5 6)))
  (check-equal? (tp-tref (tp-tref test-nested-lt 0) 2) 3)
  (check-exn exn:fail? (λ () (tp-tref (tp-tref test-nested-lt 2) 0)) 3)
  (check-exn exn:fail? (λ () (tp-tref test-nested-lt 2)) 3)
  (check-exn exn:fail? (λ () (tensor test-nested-lt test-nested-lt test-lt)))

  (check-equal? (tp-tlen test-lt) 3)
  (check-equal? (tp-tlen test-nested-lt) 2)

  (define test-lt-from-list (list->tpromise '(5 6 7 8)))
  (check-equal? (flat-store (tp-force test-lt-from-list)) (vector 5 6 7 8))
  (define test-nested-lt-from-list
    (list->tpromise `(,test-lt ,test-lt ,test-lt)))
  (check-equal? (tpromise-shape test-nested-lt-from-list) '(3 3))

  (check-true (bounded-idx*? test-nested-lt-from-list (list 0 1)))
  (check-false (bounded-idx*? test-nested-lt-from-list (list 1 3)))
  (check-false (bounded-idx*? test-nested-lt-from-list (list 1 1 0)))

  (define test-premap-lt (tensor (tensor 1 2 3) (tensor 4 5 6)))
  (define test-mapped-lt (tp-tmap add1 test-premap-lt))
  (check-true (flat? (tpromise-tensor test-premap-lt)))
  (check-true (tcomp? (tpromise-tensor test-mapped-lt)))
  (check-equal? (flat-store (tp-force test-mapped-lt)) (vector 2 3 4 5 6 7))
  (check-equal? (flat-shape (tp-force test-mapped-lt)) (flat-shape (tp-force test-premap-lt)))
  (check-equal? (flat-offset (tp-force test-mapped-lt)) (flat-offset (tp-force test-premap-lt)))
  (check-true (flat? (tpromise-tensor test-premap-lt)))
  (check-true (flat? (tpromise-tensor test-mapped-lt)))

  (define test-build-shape '(4 3))
  (define test-built-tensor (build-tpromise test-build-shape
                                           (λ (i)
                                             (let ([row (car i)]
                                                   [column (cadr i)])
                                               (+ (* (sub1 (car test-build-shape))
                                                     row)
                                                  column)))))
  (check-equal? (tpromise-shape test-built-tensor) test-build-shape)
  (check-true (tcomp? (tpromise-tensor test-built-tensor)))

  (define test-refs '(0 2))
  (define test-tp-trefs (tp-trefs test-built-tensor test-refs))
  (check-true (tcomp? (tpromise-tensor test-tp-trefs)))
  (check-equal? (tpromise-shape test-tp-trefs) (flat-shape (tp-force test-tp-trefs)))
  (check-equal? (flat-store (tp-force test-tp-trefs)) (vector 0 1 2 6 7 8))
  (check-exn exn:fail? (λ () (tp-trefs test-nested-lt '(0 4))) 3)

  (define sum-f
    (λ (in-v iᵢ sᵢ out-v iₒ sₒ)
      (vset! out-v iₒ
             (for/fold ([sum 0.0]) ([i (in-range iᵢ (+ iᵢ sᵢ))])
               (+ sum (vref in-v i))))))

  (define sum (tp-ext1-ρ sum-f 1))
  (check-equal? (flat-store (tp-force (sum test-nested-lt))) (vec 6.0 15.0))

  (define t0
    (build-tpromise '(2 3 4)
                    (λ (i)
                      (match-define `(,x ,y ,z) i)
                      (* 2 (+ (* x 12) (* y 4) (* 1 z))))))
  (define *-ρ (tp-ext2-ρ * 0 0))
  (define t0sqr (*-ρ t0 t0))

  (flat:check-tensor-equal? (tp-force t0sqr)
                            (flat:reshape
                             '(2 3 4)
                             (flat:tensor
                              0  4 16 36
                              64 100 144 196
                              256 324 400 484
                              576 676 784 900
                              1024 1156 1296 1444
                              1600 1764 1936 2116)))

  (define *-2-1-f
    (λ (v0 i0 s0 v1 i1 s1 vout iout sout)
      (for ([j0 (in-range 0 s0)])
        (vset! vout (+ iout j0)
          (* (vref v0 (+ i0 j0))
             (vref v1 (+ i1 (modulo j0 s1))))))))

  (define t1
    (build-tpromise '(5 6)
                    (λ (i)
                      (match-define `(,x ,y) i)
                      (* 2.0 (+ (* x 6) y)))))

  (define t2
    (build-tpromise '(6)
                    (λ (i) (* 3.0 (car i)))))

  (define *-2-1
    (tp-ext2-ρ *-2-1-f 2 1 (λ (s0 s1) s0)))

  (define r-1-2
    (*-2-1 t1 t2))

  (check-equal? (tpromise-shape r-1-2) '(5 6))
  (flat:check-tensor-equal? (tp-force r-1-2)
                            (flat:reshape
                             '(5 6)
                             (flat:tensor
                               0 6.0 24.0 54.0 96.0 150.0
                               0 42.0 96.0 162.0 240.0 330.0
                               0 78.0 168.0 270.0 384.0 510.0
                               0 114.0 240.0 378.0 528.0 690.0
                               0 150.0 312.0 486.0 672.0 870.0)))

  (define t3
    (build-tpromise '(3 5 6)
                    (λ (i)
                      (match-define `(,x ,y ,z) i)
                      (* 2.0 (+ (* x 30) (* y 6) (* 1 z))))))

  (define t4
    (build-tpromise '(3 6)
                    (λ (i)
                      (match-define `(,x ,y) i)
                      (* 3.0 (+ (* x 6) y)))))

  (define r-3-4
    (*-2-1 t3 t4))

  (check-equal? (tpromise-shape r-3-4) '(3 5 6))
  (flat:check-tensor-equal? (tp-force r-3-4)
   (flat:reshape
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
     6048.0 6630.0 7224.0 7830.0 8448.0 9078.0)))

  (define r1-td (tensor 3.0 4.0 5.0))
  (define r2-td (tp-reshape '(2 3) (tensor 3.0 4.0 5.0 7.0 8.0 9.0)))

  (define +ᶠ +)
  (define +ᵈ (λ (a b z) (values z z)))

  (define sqrᶠ (λ (a) (* a a)))
  (define sqrᵈ
    (λ (a z) (* z 2 a)))

  (define d-sqr (tp-ext1-∇ sqrᵈ 0 scalar-shape))

  (define one-like
    (λ (t)
      (build-tpromise (tpromise-shape t) (λ (_) 1.0))))

  (flat:check-tensor-equal? (tp-force (d-sqr r1-td (one-like r1-td)))
                            (flat:tensor 6.0 8.0 10.0))

  (let ((gsqr (d-sqr r2-td (one-like r2-td))))
    (flat:check-tensor-equal? (tp-force gsqr)
                              (flat:reshape
                               '(2 3)
                               (flat:tensor 6.0 8.0 10.0 14.0 16.0 18.0))))

  (define d+ (tp-ext2-∇ +ᵈ 0 0 scalar-shape))

  (let-values (((da db) (d+ r1-td r1-td (one-like r1-td))))
    (flat:check-tensor-equal? (tp-force da)
                              (flat:tensor 1.0 1.0 1.0))
    (flat:check-tensor-equal? (tp-force db)
                              (flat:tensor 1.0 1.0 1.0)))

  (let-values (((da db) (d+ r1-td r2-td (one-like r2-td))))
    (flat:check-tensor-equal? (tp-force da)
                              (flat:tensor 2.0 2.0 2.0))
    (flat:check-tensor-equal? (tp-force db)
                              (flat:reshape
                               '(2 3)
                               (flat:tensor 1.0 1.0 1.0 1.0 1.0 1.0))))

  (define *∇ (tp-ext2-∇ (λ (a b z) (values (* z b) (* z a)))
                        0
                        0))

  (let-values (((gt gu) (*∇ (tensor 2.0 3.0 4.0)
                            (tensor 1.0 2.0 3.0)
                            (tensor 1.0 1.0 1.0))))
    (flat:check-tensor-equal? (tp-force gt) (tp-force (tensor 1.0 2.0 3.0)))
    (flat:check-tensor-equal? (tp-force gu) (tp-force (tensor 2.0 3.0 4.0))))

  (define sum-1-∇
    (λ (g t it st vz iz sz)
      (for* ([i (in-range it (+ it st))])
        (vset! g i (vref vz iz)))))

  (define sum-∇ (tp-ext1-∇ sum-1-∇ 1 (λ (s) '())))

  (let ((gt (sum-∇ (tensor 2.0 3.0 4.0)
                   1.0)))
    (flat:check-tensor-equal? (tp-force gt) (tp-force (tensor 1.0 1.0 1.0))))

  (let ((gt (sum-∇ (tensor (tensor 2.0 3.0 4.0)
                           (tensor 2.0 3.0 4.0))
                   (tensor 2.0 1.0))))
    (flat:check-tensor-equal? (tp-force gt) (tp-force (tensor (tensor 2.0 2.0 2.0)
                                                              (tensor 1.0 1.0 1.0))))))
