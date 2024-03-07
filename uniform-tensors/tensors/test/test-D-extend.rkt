(module+ test
  (require rackunit)
  (require "A-equality.rkt")
  (require "B-tensor-basics.rkt")

  (define sum-f
    (λ (in-v iᵢ sᵢ out-v iₒ sₒ)
      (vset! out-v iₒ
        (for/fold ([sum 0.0]) ([i (in-range iᵢ (+ iᵢ sᵢ))])
          (+ sum (vref in-v i))))))

  (define sum-shape-f
    (λ (in-f-shape)
      '()))

  (define sum (ext1-ρ sum-f 1 sum-shape-f))

  (check-equal? (min-shape 2 '(3 4 5 6)) '(5 6))

  (check-equal? (min-shape 0 '(3 4 5 6)) '())

  (check-equal? (merge-shapes '(3 4 5 6) 1 '())
                '(3 4 5))

  (define t0
    (flat '(2 3 4)
          (build-vec 24
            (λ (i)
              (* 2 i)))
          0))

  (check-true (equal-elements? (sum t0)
                               (tensor 12.0 44.0 76.0 108.0 140.0 172.0)))


  (define dup-f
    (λ (in-v iᵢ sᵢ out-v iₒ sₒ)
      (for ([i (in-range 0 sₒ)])
        (vset! out-v (+ iₒ i)
          (vref in-v (+ iᵢ (modulo i sᵢ)))))))

  (define dup-shape-f
    (λ (in-f-shape)
      (list (* 2 (car in-f-shape)))))

  (define dup (ext1-ρ dup-f 1 dup-shape-f))
  (check-true (equal-elements? (dup t0)
                               (tensor 0 2 4 6 0 2 4 6
                                       8 10 12 14 8 10 12 14
                                       16 18 20 22 16 18 20 22
                                       24 26 28 30 24 26 28 30
                                       32 34 36 38 32 34 36 38
                                       40 42 44 46 40 42 44 46)))

  (define s0 '(3 4 5 6))
  (define s1 '(3 7 6))
  (define r0 2)
  (define r1 1)

  (ext2-shapes s0 s1 r0 r1 '(5 6)
     (λ (s-out size-out q0 q1 strides)
       (check-equal? s-out '(3 4 7 5 6))
       (check-equal? size-out 2520)
       (check-equal? strides '(#(840 120 42) #(210 30 0) #(30 0 6)))
       (let-values (((i0 i1) (idxs strides 0 0 0)))
         (check-equal? i0 0)
         (check-equal? i1 0))

       (let-values (((i0 i1) (idxs strides 30 0 0)))
         (check-equal? i0 0)
         (check-equal? i1 6))

       (let-values (((i0 i1) (idxs strides 210 0 0)))
         (check-equal? i0 30)
         (check-equal? i1 0))

       (let-values (((i0 i1) (idxs strides 240 0 0)))
         (check-equal? i0 30)
         (check-equal? i1 6))

       (let-values (((i0 i1) (idxs strides 420 0 0)))
         (check-equal? i0 60)
         (check-equal? i1 0))

       (let-values (((i0 i1) (idxs strides 840 0 0)))
         (check-equal? i0 120)
         (check-equal? i1 42))
       ))


  (define *-ρ (ext2-ρ * 0 0))
  (define t0sqr (*-ρ t0 t0))

  (check-true (equal-elements?
               t0sqr
               (tensor 0  4 16 36
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
    (flat '(5 6)
          (build-vec 30
            (λ (i) (* 2.0 i)))
          0))

  (define t2
    (flat '(6)
          (build-vec 6
            (λ (i) (* 3.0 i)))
          0))

  (define *-2-1
    (ext2-ρ *-2-1-f 2 1 (λ (s0 s1) s0)))

  (define r-1-2
    (*-2-1 t1 t2))

  (check-tensor-equal? r-1-2
                       (reshape
                        '(5 6)
                        (tensor 0 6.0 24.0 54.0 96.0 150.0
                               0 42.0 96.0 162.0 240.0 330.0
                               0 78.0 168.0 270.0 384.0 510.0
                               0 114.0 240.0 378.0 528.0 690.0
                               0 150.0 312.0 486.0 672.0 870.0)))

  (define t3
    (flat '(3 5 6)
          (build-vec 90
            (λ (i) (* 2.0 i)))
          0))

  (define t4
    (flat '(3 6)
          (build-vec 18
            (λ (i) (* 3.0 i)))
          0))

  (define r-3-4
    (*-2-1 t3 t4))

  (check-tensor-equal? r-3-4
                       (reshape
                        '(3 5 6)
                        (tensor
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

(module+ test
  (require rackunit)

  (define r0-td 3.0)
  (define r1-td (flat '(3) (list->vec '(3.0 4.0 5.0)) 0))
  (define r2-td (flat '(2 3) (list->vec '(3.0 4.0 5.0 7.0 8.0 9.0)) 0))

  (define +ᶠ +)
  (define +ᵈ (λ (a b z) (values z z)))

  (define sqrᶠ (λ (a) (* a a)))
  (define sqrᵈ
    (λ (a z) (* z 2 a)))

  (define d-sqr (ext1-∇ sqrᵈ 0 scalar-shape))

  (define one-like
    (λ (t)
      (let* ((st (flat-shape t))
             (size-t (size-of st)))
        (flat st
              (new-vec size-t 1.0)
              0))))

  (check-true (equal-elements? (d-sqr r1-td (one-like r1-td)) (tensor 6.0 8.0 10.0)))

  (let ((gsqr (d-sqr r2-td (one-like r2-td))))
    (check-tensor-equal? gsqr (reshape '(2 3) (tensor 6.0 8.0 10.0 14.0 16.0 18.0))))

  (define d+ (ext2-∇ +ᵈ 0 0 scalar-shape))

  (let-values (((da db) (d+ r1-td r1-td (one-like r1-td))))
    (check-tensor-equal? da (tensor 1.0 1.0 1.0))
    (check-tensor-equal? db (tensor 1.0 1.0 1.0)))

  (let-values (((da db) (d+ r1-td r2-td (one-like r2-td))))
    (check-tensor-equal? da (tensor 2.0 2.0 2.0))
    (check-tensor-equal? db (reshape '(2 3) (tensor 1.0 1.0 1.0 1.0 1.0 1.0))))

  (define *∇ (ext2-∇ (λ (a b z) (values (* z b) (* z a)))
                       0
                       0))

  (let-values (((gt gu) (*∇ (tensor 2.0 3.0 4.0) (tensor 1.0 2.0 3.0) (tensor 1.0 1.0 1.0))))
    (check-tensor-equal? gt (tensor 1.0 2.0 3.0))
    (check-tensor-equal? gu (tensor 2.0 3.0 4.0)))

  (define sum-1-∇
    (λ (g t it st vz iz sz)
      (for* ([i (in-range it (+ it st))])
        (vset! g i (vref vz iz)))))

  (define sum-∇ (ext1-∇ sum-1-∇ 1 (λ (s) '())))

  (let ((gt (sum-∇ (tensor 2.0 3.0 4.0)
                   1.0)))
    (check-tensor-equal? gt (tensor 1.0 1.0 1.0)))

  (let ((gt (sum-∇ (tensor (tensor 2.0 3.0 4.0)
                           (tensor 2.0 3.0 4.0))
                   (tensor 2.0 1.0))))
    (check-tensor-equal? gt (tensor (tensor 2.0 2.0 2.0)
                                    (tensor 1.0 1.0 1.0)))))
