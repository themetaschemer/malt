(module+ test
  (require rackunit)

  ;; TODO: Add a comment above each test case describing what the test case is testing
  (define-check (check-compiler-invariants tp)
    (let-values (((instrs locals env) (run-compiler
                                       (compile-expr tp
                                                     (count-references tp (hasheq)))
                                       '() '())))
      (with-check-info
          (('env (nested-info
                  (map (λ (name/flat)
                         (make-check-info (car name/flat) (cdr name/flat)))
                       env)))
           ('instrs instrs))
        (for ((name/flat env))
          (unless (and (flat:flat? (cdr name/flat))
                       (not (null? (flat-shape (cdr name/flat)))))
            (fail-check (format (string-append "Value associated with the variable"
                                               " ~a should be a flat tensor. "
                                               "Associated value found: ~a")
                                (car name/flat) (cdr name/flat)))))
        (define unique-flats (list->seteq (map cdr env)))
        (unless (equal? (set-count unique-flats)
                        (length (filter flat? (map cdr env))))
          (fail-check (string-append "Duplicate flat tensors found"
                                     " in environment. Variables in environment"
                                     " should be paired with unique"
                                     " flat tensors"))))))

  (define test-lt (tensor 1 2 3))
  (check-compiler-invariants test-lt)
  (check-true (flat? (tpromise-tensor test-lt)))
  (check-equal? (flat-store (force/eval test-lt)) (vector 1 2 3))
  (check-true (flat? (tpromise-tensor test-lt)))
  (check-exn exn:fail? (λ () (tensor test-lt 4)))
  (check-exn exn:fail? (λ () (tensor 4 test-lt)))

  (define test-tcomp-tref (tp-tref test-lt 2))
  (check-compiler-invariants test-tcomp-tref)
  (check-equal? (force/eval test-tcomp-tref) 3)
  (check-exn exn:fail? (λ () (tp-tref test-lt 5)))

  (define test-nested-lt (tensor (tensor 1 2 3) (tensor 4 5 6)))
  (define test-tcomp-tref-nested (tp-tref (tp-tref test-nested-lt 0) 2))
  (check-compiler-invariants test-tcomp-tref-nested)
  (check-equal? (force/eval test-tcomp-tref-nested) 3)
  (check-exn exn:fail? (λ () (tp-tref (tp-tref test-nested-lt 2) 0)))
  (check-exn exn:fail? (λ () (tp-tref test-nested-lt 2)))
  (check-exn exn:fail? (λ () (tensor test-nested-lt test-nested-lt test-lt)))

  (check-equal? (tp-tlen test-lt) 3)
  (check-equal? (tp-tlen test-nested-lt) 2)

  (define test-lt-from-list (list->tpromise '(5 6 7 8)))
  (check-compiler-invariants test-lt-from-list)
  (check-equal? (flat-store (force/eval test-lt-from-list)) (vector 5 6 7 8))
  (define test-nested-lt-from-list
    (list->tpromise `(,test-lt ,test-lt ,test-lt)))
  (check-compiler-invariants test-nested-lt-from-list)
  (check-equal? (flat-store (force/eval test-nested-lt-from-list))
                (vector 1 2 3 1 2 3 1 2 3))
  (check-equal? (tpromise-shape test-nested-lt-from-list) '(3 3))

  (check-true (bounded-idx*? test-nested-lt-from-list (list 0 1)))
  (check-false (bounded-idx*? test-nested-lt-from-list (list 1 3)))
  (check-false (bounded-idx*? test-nested-lt-from-list (list 1 1 0)))

  (define test-tcomp-partial-eval
    (begin
      (force/eval test-nested-lt-from-list)
      (force/eval test-nested-lt)
      (force/eval test-lt)
      (tp-tref
       (tp-tref (tensor (tensor (tensor 1 2 3) (tensor 4 5 6) (tensor 7 8 9))
                        test-nested-lt-from-list
                        (list->tpromise (list (tp-tref test-nested-lt 0)
                                              (tp-tref test-nested-lt 1)
                                              test-lt)))
                1)
       2)))
  (check-compiler-invariants test-tcomp-partial-eval)
  (flat:check-tensor-equal? (force/eval test-tcomp-partial-eval)
                            (force/eval (tensor 1 2 3)))

  (define test-build-shape '(4 3))
  (define test-built-tensor (build-tpromise test-build-shape
                                            (λ (i)
                                              (let ([row (car i)]
                                                    [column (cadr i)])
                                                (+ (* (sub1 (car test-build-shape))
                                                      row)
                                                   column)))))
  (check-compiler-invariants test-built-tensor)
  (check-equal? (tpromise-shape test-built-tensor) test-build-shape)
  (check-true (tcomp? (tpromise-tensor test-built-tensor)))
  (flat:check-tensor-equal? (force/eval test-built-tensor)
                            (force/eval (tensor (tensor 0 1 2)
                                                 (tensor 3 4 5)
                                                 (tensor 6 7 8)
                                                 (tensor 9 10 11))))

  (define test-refs '(0 2))
  (define test-tp-trefs (tp-trefs test-built-tensor test-refs))
  (check-compiler-invariants test-tp-trefs)
  (check-true (tcomp? (tpromise-tensor test-tp-trefs)))
  (check-equal? (tpromise-shape test-tp-trefs)
                (flat-shape (force/eval test-tp-trefs)))
  (flat:check-tensor-equal? (force/eval test-tp-trefs)
                            (force/eval (tensor (tensor 0 1 2)
                                                 (tensor 6 7 8))))
  (check-exn exn:fail? (λ () (tp-trefs test-nested-lt '(0 4))))

  (define test-tp-reshape (tp-reshape '(3 2 1) (tp-trefs test-built-tensor '(1 3))))
  (check-compiler-invariants test-tp-reshape)
  (flat:check-tensor-equal? (force/eval test-tp-reshape)
                            (force/eval (tensor (tensor (tensor 3)
                                                         (tensor 4))
                                                 (tensor (tensor 5)
                                                         (tensor 9))
                                                 (tensor (tensor 10)
                                                         (tensor 11)))))
  (check-exn exn:fail? (λ () (tp-reshape '(4 5) test-tp-reshape)))

  (define sum-f
    (λ (in-v iᵢ sᵢ out-v iₒ sₒ)
      (vset! out-v iₒ
             (for/fold ([sum 0.0]) ([i (in-range iᵢ (+ iᵢ sᵢ))])
               (+ sum (vref in-v i))))))

  (define sum (tp-ext1-ρ sum-f 1))
  (define test-tp-sum (sum test-nested-lt))
  (check-compiler-invariants test-tp-sum)
  (flat:check-tensor-equal? (force/eval test-tp-sum)
                            (force/eval (tensor 6.0 15.0)))

  (define test-tp-sum-nested (tensor 4.0 (sum (tensor 1 2 3)) 5.0))
  (check-compiler-invariants test-tp-sum-nested)
  (flat:check-tensor-equal? (force/eval test-tp-sum-nested)
                            (force/eval (tensor 4.0 6.0 5.0)))

  (define id-f (lambda (v) v))
  (define id-ρ (tp-ext1-ρ id-f 1 (λ (s) s)))
  (define test-tp-id (id-ρ test-nested-lt))
  (check-compiler-invariants test-tp-id)
  (flat:check-tensor-equal? (force/eval test-tp-id)
                            (force/eval (tensor (tensor 1 2 3)
                                                 (tensor 4 5 6))))

  (define test-tp-id-scalar (id-ρ (sum (tensor 4 5 6))))
  (check-compiler-invariants test-tp-id-scalar)
  (check-equal? (force/eval test-tp-id-scalar) 15.0)

  (define t0
    (build-tpromise '(2 3 4)
                    (λ (i)
                      (match-define `(,x ,y ,z) i)
                      (* 2 (+ (* x 12) (* y 4) (* 1 z))))))
  (define *-ρ (tp-ext2-ρ * 0 0))
  (define t0sqr (*-ρ t0 t0))

  (check-compiler-invariants t0sqr)
  (flat:check-tensor-equal? (force/eval t0sqr)
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
  (check-compiler-invariants r-1-2)
  (flat:check-tensor-equal? (force/eval r-1-2)
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
  (check-compiler-invariants r-3-4)
  (flat:check-tensor-equal? (force/eval r-3-4)
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

  (define r-sum-2-scalar (*-ρ (sum t2) (sum (tensor 2 3 4))))
  (check-compiler-invariants r-sum-2-scalar)
  (flat:check-tensor-equal? (force/eval r-sum-2-scalar) 405.0)

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

  (define tcomp-dsqr-r1 (d-sqr r1-td (one-like r1-td)))
  (check-compiler-invariants tcomp-dsqr-r1)
  (flat:check-tensor-equal? (force/eval tcomp-dsqr-r1)
                            (flat:tensor 6.0 8.0 10.0))

  (let ((gsqr (d-sqr r2-td (one-like r2-td))))
    (check-compiler-invariants gsqr)
    (flat:check-tensor-equal? (force/eval gsqr)
                              (flat:reshape
                               '(2 3)
                               (flat:tensor 6.0 8.0 10.0 14.0 16.0 18.0))))

  (define d+ (tp-ext2-∇ +ᵈ 0 0 scalar-shape))

  (let-values (((da db) (d+ 2.0 3.0 1.0)))
    (check-compiler-invariants da)
    (flat:check-tensor-equal? (force/eval da) 1.0)
    (check-compiler-invariants db)
    (flat:check-tensor-equal? (force/eval db) 1.0))

  (let-values (((da db) (d+ r1-td r1-td (one-like r1-td))))
    (check-compiler-invariants da)
    (flat:check-tensor-equal? (force/eval da)
                              (flat:tensor 1.0 1.0 1.0))
    (check-compiler-invariants db)
    (flat:check-tensor-equal? (force/eval db)
                              (flat:tensor 1.0 1.0 1.0)))

  (let-values (((da db) (d+ r1-td r2-td (one-like r2-td))))
    (check-compiler-invariants da)
    (flat:check-tensor-equal? (force/eval da)
                              (flat:tensor 2.0 2.0 2.0))
    (check-compiler-invariants db)
    (flat:check-tensor-equal? (force/eval db)
                              (flat:reshape
                               '(2 3)
                               (flat:tensor 1.0 1.0 1.0 1.0 1.0 1.0))))

  (define *∇ (tp-ext2-∇ (λ (a b z) (values (* z b) (* z a)))
                        0
                        0))

  (let-values (((gt gu) (*∇ (tensor 2.0 3.0 4.0)
                            (tensor 1.0 2.0 3.0)
                            (tensor 1.0 1.0 1.0))))
    (check-compiler-invariants gt)
    (check-compiler-invariants gu)
    (flat:check-tensor-equal? (force/eval gt) (force/eval (tensor 1.0 2.0 3.0)))
    (flat:check-tensor-equal? (force/eval gu) (force/eval (tensor 2.0 3.0 4.0))))

  (define sum-1-∇
    (λ (g t it st vz iz sz)
      (for* ([i (in-range it (+ it st))])
        (vset! g i (vref vz iz)))))

  (define sum-∇ (tp-ext1-∇ sum-1-∇ 1 (λ (s) '())))

  (let ((gt (sum-∇ (tensor 2.0 3.0 4.0)
                   1.0)))
    (check-compiler-invariants gt)
    (flat:check-tensor-equal? (force/eval gt) (force/eval (tensor 1.0 1.0 1.0))))

  (let ((gt (sum-∇ (tensor (tensor 2.0 3.0 4.0)
                           (tensor 2.0 3.0 4.0))
                   (tensor 2.0 1.0))))
    (check-compiler-invariants gt)
    (flat:check-tensor-equal? (force/eval gt) (force/eval (tensor (tensor 2.0 2.0 2.0)
                                                                    (tensor 1.0 1.0 1.0)))))
  ;; t and u must have the same shape
  (define s2-f (lambda (t u) (tensor (sum t) (sum u))))
  (define s2-d
    (λ (g0 g1 t it st u iu su vz iz sz)
      (for* ([i (in-range it (+ it st))])
        (vset! g0 i (vref vz iz))
        (vset! g1 i (vref vz (+ iz 1))))))
  (define s2-∇ (tp-ext2-∇ s2-d 1 1 (λ (s0 s1) (list 2))))
  (let-values (((gt gu) (s2-∇ (tensor 2.0 3.0 4.0)
                              (tensor 1.0 2.0 3.0)
                              (tensor 1.0 1.0))))
    (check-compiler-invariants gt)
    (check-compiler-invariants gu)
    (flat:check-tensor-equal? (force/eval gt) (force/eval (tensor 1.0 1.0 1.0)))
    (flat:check-tensor-equal? (force/eval gu) (force/eval (tensor 1.0 1.0 1.0))))
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
    (flat:check-tensor-equal? (force/eval gt) (force/eval (tp-reshape '(3 2 3) (list->tpromise (make-list 18 1.0)))))
    (flat:check-tensor-equal? (force/eval gu) (force/eval (tp-reshape '(3 2 3) (list->tpromise (make-list 18 1.0))))))

  (define test-env-flat-scalar ((λ (theta) (*-ρ (list-ref theta 0) (list-ref theta 1))) (list (tensor 1.0) 3.0)))
  (check-compiler-invariants test-env-flat-scalar)

  ;; Check common subexpression introduced by let is not repeated
  ;; TODO: add a generic version of the next 2 tests in check-compiler-invariants
  (define count-flat:tref
    (λ (ls)
      (cond
        ((null? ls) 0)
        ((pair? (car ls)) (+ (count-flat:tref (car ls))
                             (count-flat:tref (cdr ls))))
        ((eqv? (car ls) 'flat:tref)
         (add1 (count-flat:tref (cdr ls))))
        (else (count-flat:tref (cdr ls))))))
  (define test-common-subexpr
    (let ((t (tp-tref (tensor 1 2 3) 0)))
      (tensor t t)))
  (let-values (((instrs locals env) (run-compiler
                                     (compile-expr test-common-subexpr
                                                   (count-references
                                                    test-common-subexpr
                                                    (hasheq)))
                                     '() '())))
    (check-equal? (count-flat:tref (make-instrs instrs locals env)) 1
                  "Common subexpression containing flat:tref should occur once"))
  (define test-common-nested-subexprs
    (let ((t1 (tp-tref (tensor (tensor 1 2 3) (tensor 4 5 6)) 0)))
      (let ((t0 (tp-tref t1 0)))
        (tensor t0 t0))))
  (let-values (((instrs locals env) (run-compiler
                                     (compile-expr test-common-nested-subexprs
                                                   (count-references
                                                    test-common-nested-subexprs
                                                    (hasheq)))
                                     '() '())))
    (check-equal? (count-flat:tref (make-instrs instrs locals env)) 2
                  "Common subexpressions containing flat:tref should occur twice"))

  (define random-tensor
    (λ (s)
      (build-tpromise s (λ (tidx) (random 10)))))
  (define test-build-random
    (let ((v (random-tensor '(3 2 4))))
      (*-ρ v v)))
  (check-pred
   (λ (fs) (andmap (λ (e) (integer? (sqrt e))) fs))
   (vector->list (flat:flat-store (force/eval test-build-random)))))
