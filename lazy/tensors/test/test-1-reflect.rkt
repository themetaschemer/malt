(module+ test
  (require rackunit)
  (require (only-in "c3-compiler.rkt"
                    compile-tensor/checks))
  (require "0-lazy.rkt")
  (require "B-test-programs.rkt")
  ;; TODO: Add a comment above each test case describing what the test case is testing
  (define-check (check-compiler-invariants tp)
    (let-values (((instrs ds) (compile-tensor tp)))
      (with-check-info
          (('data-segment ds)
           ('instrs instrs))
        'ok
        #;
        (for ((name/flat ds))
          (unless (and (flat:flat? (cdr name/flat))
                       (not (null? (flat:flat-shape (cdr name/flat)))))
            (fail-check (format (string-append "Value associated with the variable"
                                               " ~a should be a flat tensor. "
                                               "Associated value found: ~a")
                                (car name/flat) (cdr name/flat)))))
        #;
        (define unique-flats (list->seteq (map cdr ds)))
        #;
        (unless (equal? (set-count unique-flats)
                        (length (filter flat? (map cdr ds))))
          (fail-check (string-append "Duplicate flat tensors found"
                                     " in data segment. Variables in data segment"
                                     " should be paired with unique"
                                     " flat tensors"))))))
  ;;TODO: Move all check-compiler-invariant checks to the test file for
  ;;c3-compiler.rkt file.

  ;;TODO: Refactor all test cases to use get-test-program so that ↓ doesn't
  ;;mutate the programs defined in B-test-programs
  (define test-tensor-r1-0 (get-test-program 'tensor-r1-0))
  (check-compiler-invariants test-tensor-r1-0)
  (check-true (flat:flat? (tpromise-tensor test-tensor-r1-0)))
  (flat:check-tensor-equal? (↓ test-tensor-r1-0)
                            (get-test-eval-res 'tensor-r1-0))
  (check-true (flat:flat? (tpromise-tensor test-tensor-r1-0)))
  (check-exn exn:fail? (λ () (tensor test-tensor-r1-0 4)))
  (check-exn exn:fail? (λ () (tensor 4 test-tensor-r1-0)))

  (check-compiler-invariants test-tcomp-tref)
  (check-equal? (↓ test-tcomp-tref) 3)
  (check-exn exn:fail? (λ () (tref test-tensor-r1-0 5)))

  (define test-nested-tensor (get-test-program 'tensor-r2-0))
  (check-compiler-invariants test-tcomp-tref-nested)
  (check-equal? (↓ test-tcomp-tref-nested) 3)
  (check-exn exn:fail? (λ () (tref (tref test-nested-tensor 2) 0)))
  (check-exn exn:fail? (λ () (tref test-nested-tensor 2)))
  (check-exn exn:fail? (λ () (tensor test-nested-tensor test-nested-tensor test-tensor-r1-0)))

  (check-equal? (tlen test-tensor-r1-0) 3)
  (check-equal? (tlen test-nested-tensor) 2)

  (check-compiler-invariants test-list->tensor)
  (check-equal? (flat:flat-store (↓ test-list->tensor)) (vector 5 6 7 8))
  (check-compiler-invariants test-nested-list->tensor)
  (check-equal? (flat:flat-store (↓ test-nested-list->tensor))
                (vector 1 2 3 1 2 3 1 2 3))
  (check-equal? (tpromise-shape test-nested-list->tensor) '(3 3))

  (define test-tcomp-partial-eval
    (begin
      (↓ test-nested-list->tensor)
      (↓ test-nested-tensor)
      (↓ test-tensor-r1-0)
      (tref
       (tref (tensor (tensor (tensor 1 2 3) (tensor 4 5 6) (tensor 7 8 9))
                        test-nested-list->tensor
                        (list->tensor (list (tref test-nested-tensor 0)
                                              (tref test-nested-tensor 1)
                                              test-tensor-r1-0)))
                1)
       2)))
  (check-compiler-invariants test-tcomp-partial-eval)
  (flat:check-tensor-equal? (↓ test-tcomp-partial-eval)
                            (↓ (tensor 1 2 3)))

  (check-compiler-invariants test-built-tensor)
  (check-equal? (tpromise-shape test-built-tensor) test-build-shape)
  (check-true (tcomp? (tpromise-tensor test-built-tensor)))
  (flat:check-tensor-equal? (↓ test-built-tensor)
                            (↓ (tensor (tensor 0 1 2)
                                       (tensor 3 4 5)
                                       (tensor 6 7 8)
                                       (tensor 9 10 11))))

  (check-compiler-invariants test-trefs)
  (check-true (tcomp? (tpromise-tensor test-trefs)))
  (check-equal? (tpromise-shape test-trefs)
                (flat:flat-shape (↓ test-trefs)))
  (flat:check-tensor-equal? (↓ test-trefs)
                            (↓ (tensor (tensor 0 1 2)
                                       (tensor 6 7 8))))
  (check-exn exn:fail? (λ () (trefs test-nested-tensor '(0 4))))

  (check-compiler-invariants test-reshape)
  (flat:check-tensor-equal? (↓ test-reshape)
                            (↓ (tensor (tensor (tensor 3)
                                               (tensor 4))
                                       (tensor (tensor 5)
                                               (tensor 9))
                                       (tensor (tensor 10)
                                               (tensor 11)))))
  (check-exn exn:fail? (λ () (reshape '(4 5) test-reshape)))

  (check-compiler-invariants test-tp-sum)
  (flat:check-tensor-equal? (↓ test-tp-sum)
                            (↓ (tensor 6.0 15.0)))

  (check-compiler-invariants test-tp-sum-nested)
  (flat:check-tensor-equal? (↓ test-tp-sum-nested)
                            (↓ (tensor 4.0 6.0 5.0)))

  (check-compiler-invariants test-tp-id)
  (flat:check-tensor-equal? (↓ test-tp-id)
                            (↓ (tensor (tensor 1 2 3)
                                       (tensor 4 5 6))))

  (check-compiler-invariants test-tp-id-scalar)
  (check-equal? (↓ test-tp-id-scalar) 15.0)

  (check-compiler-invariants t0sqr)
  (flat:check-tensor-equal? (↓ t0sqr)
                            (flat:reshape
                             '(2 3 4)
                             (flat:tensor
                              0  4 16 36
                              64 100 144 196
                              256 324 400 484
                              576 676 784 900
                              1024 1156 1296 1444
                              1600 1764 1936 2116)))

  (check-equal? (tpromise-shape r-1-2) '(5 6))
  (check-compiler-invariants r-1-2)
  (flat:check-tensor-equal? (↓ r-1-2)
                            (flat:reshape
                             '(5 6)
                             (flat:tensor
                              0 6.0 24.0 54.0 96.0 150.0
                              0 42.0 96.0 162.0 240.0 330.0
                              0 78.0 168.0 270.0 384.0 510.0
                              0 114.0 240.0 378.0 528.0 690.0
                              0 150.0 312.0 486.0 672.0 870.0)))

  (check-equal? (tpromise-shape r-3-4) '(3 5 6))
  (check-compiler-invariants r-3-4)
  (flat:check-tensor-equal? (↓ r-3-4)
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

  (check-compiler-invariants r-sum-2-scalar)
  (flat:check-tensor-equal? (↓ r-sum-2-scalar) 405.0)

  (check-compiler-invariants tcomp-dsqr-r1)
  (flat:check-tensor-equal? (↓ tcomp-dsqr-r1)
                            (flat:tensor 6.0 8.0 10.0))

  (let ((gsqr (d-sqr r2-td (one-like r2-td))))
    (check-compiler-invariants gsqr)
    (flat:check-tensor-equal? (↓ gsqr)
                              (flat:reshape
                               '(2 3)
                               (flat:tensor 6.0 8.0 10.0 14.0 16.0 18.0))))

  (let-values (((da db) (d+ 2.0 3.0 1.0)))
    (check-compiler-invariants da)
    (flat:check-tensor-equal? (↓ da) 1.0)
    (check-compiler-invariants db)
    (flat:check-tensor-equal? (↓ db) 1.0))

  (let-values (((da db) (d+ r1-td r1-td (one-like r1-td))))
    (check-compiler-invariants da)
    (flat:check-tensor-equal? (↓ da)
                              (flat:tensor 1.0 1.0 1.0))
    (check-compiler-invariants db)
    (flat:check-tensor-equal? (↓ db)
                              (flat:tensor 1.0 1.0 1.0)))

  (let-values (((da db) (d+ r1-td r2-td (one-like r2-td))))
    (check-compiler-invariants da)
    (flat:check-tensor-equal? (↓ da)
                              (flat:tensor 2.0 2.0 2.0))
    (check-compiler-invariants db)
    (flat:check-tensor-equal? (↓ db)
                              (flat:reshape
                               '(2 3)
                               (flat:tensor 1.0 1.0 1.0 1.0 1.0 1.0))))

  (let-values (((gt gu) (*∇ (tensor 2.0 3.0 4.0)
                            (tensor 1.0 2.0 3.0)
                            (tensor 1.0 1.0 1.0))))
    (check-compiler-invariants gt)
    (check-compiler-invariants gu)
    (flat:check-tensor-equal? (↓ gt) (↓ (tensor 1.0 2.0 3.0)))
    (flat:check-tensor-equal? (↓ gu) (↓ (tensor 2.0 3.0 4.0))))

  (let ((gt (sum-∇ (tensor 2.0 3.0 4.0)
                   1.0)))
    (check-compiler-invariants gt)
    (flat:check-tensor-equal? (↓ gt) (↓ (tensor 1.0 1.0 1.0))))

  (let ((gt (sum-∇ (tensor (tensor 2.0 3.0 4.0)
                           (tensor 2.0 3.0 4.0))
                   (tensor 2.0 1.0))))
    (check-compiler-invariants gt)
    (flat:check-tensor-equal? (↓ gt) (↓ (tensor (tensor 2.0 2.0 2.0)
                                                (tensor 1.0 1.0 1.0)))))
  (let-values (((gt gu) (s2-∇ (tensor 2.0 3.0 4.0)
                              (tensor 1.0 2.0 3.0)
                              (tensor 1.0 1.0))))
    (check-compiler-invariants gt)
    (check-compiler-invariants gu)
    (flat:check-tensor-equal? (↓ gt) (↓ (tensor 1.0 1.0 1.0)))
    (flat:check-tensor-equal? (↓ gu) (↓ (tensor 1.0 1.0 1.0))))
  (let-values (((gt gu) (s2-∇ (tensor (tensor (tensor 1.0 2.0 6.0)
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
                                              (tensor 1.0 1.0))))))
    (check-compiler-invariants gt)
    (check-compiler-invariants gu)
    (flat:check-tensor-equal?
     (↓ gt)
     (↓ (reshape '(3 2 3) (list->tensor (make-list 18 1.0)))))
    (flat:check-tensor-equal?
     (↓ gu)
     (↓ (reshape '(3 2 3) (list->tensor (make-list 18 1.0))))))

  (check-compiler-invariants test-env-flat-scalar)
  (flat:check-tensor-equal? (↓ test-env-flat-scalar)
                            (flat:tensor 3.0))

  (check-compiler-invariants test-common-subexpr)
  (flat:check-tensor-equal? (↓ test-common-subexpr)
                            (flat:tensor 1.0 1.0))

  (check-compiler-invariants test-common-nested-subexprs)
  (flat:check-tensor-equal? (↓ test-common-nested-subexprs)
                            (flat:tensor 1.0 1.0))

  (check-pred
   (λ (fs) (andmap (λ (e) (integer? (sqrt e))) fs))
   (vector->list (flat:flat-store (↓ test-build-random)))))
