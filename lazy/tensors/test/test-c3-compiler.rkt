(module+ test
  (require rackunit)
  (require "B-test-programs.rkt")
  (require "0-lazy.rkt")

  (define-check (check-signatures-equal? t1 t2)
    (let-values (((eds-instrs-1 ds1) (extract-data-segment t1))
                 ((eds-instrs-2 ds2) (extract-data-segment t2)))
      (let ((sig1 (generate-signature eds-instrs-1))
            (sig2 (generate-signature eds-instrs-2)))
        (with-check-info
          (('extracted-instrs-1 eds-instrs-1)
           ('extracted-instrs-2 eds-instrs-2)
           ('data-segment-1 ds1)
           ('data-segment-2 ds2)
           ('signature-1 sig1)
           ('signature-2 sig2))
          (unless (equal? sig1 sig2)
            (fail-check "signature mismatch"))))))

  (define-check (check-signatures-not-equal? t1 t2)
    (let-values (((eds-instrs-1 ds1) (extract-data-segment t1))
                 ((eds-instrs-2 ds2) (extract-data-segment t2)))
      (let ((sig1 (generate-signature eds-instrs-1))
            (sig2 (generate-signature eds-instrs-2)))
        (with-check-info
          (('extracted-instrs-1 eds-instrs-1)
           ('extracted-instrs-2 eds-instrs-2)
           ('data-segment-1 ds1)
           ('data-segment-2 ds2)
           ('signature-1 sig1)
           ('signature-2 sig2))
          (when (equal? sig1 sig2)
            (fail-check "signatures musn't match"))))))

  (define test-tensor-r1-1 (get-test-program 'tensor-r1-1))
  (check-signatures-equal? test-tcomp-tref
                           (make-tref-test-program test-tensor-r1-1))
  (check-signatures-not-equal? test-tcomp-tref
                               (make-list->tensor-test-program `(,test-tensor-r1-1)))

  (define tensor-r1 (get-test-program 'tensor-r1-0))
  (check-signatures-equal? (*-ρ 2 tensor-r1) (*-ρ 3 tensor-r1))
  (check-signatures-not-equal? (*-ρ 2 3) (*-ρ 3 3))

  (define v^ (random-tensor (list 10 4)))
  (define r^ (random-tensor (list 10 4 2)))
  (check-signatures-equal? mean-v (mean v^))
  (check-signatures-equal? (mean (get-test-program 'tensor-r2-0))
                           (mean (tensor (tensor 12 23 44)
                                         (tensor 23 46 57))))
  (check-signatures-equal? (mean (get-test-program 'tensor-r2-0))
                           (mean (tensor (tensor 12 23 44)
                                         (tensor 23 46 57)
                                         (tensor 67 32 58))))
  (check-signatures-not-equal? (mean (get-test-program 'tensor-r2-0))
                               (mean (reshape '(2 3) (tensor 1 2 3 4 5 6))))
  (check-signatures-equal? variance-v (variance v^))
  (check-signatures-equal? mean-r (mean r^))
  (check-signatures-equal? variance-r (variance r^))
  (check-signatures-not-equal? mean-v mean-r)
  (check-signatures-not-equal? mean-v variance-v)
  (check-signatures-not-equal? variance-v  mean-r)
  (check-signatures-equal? (+-ρ mean-v (tensor 0 1 2 3 4 5 6 7 8 9))
                           (+-ρ (mean v^) (tensor 0 1 2 3 4 5 6 7 8 9)))

  (let ((a 2)
        (b 3))
    (let*-values (((da- db-) (d- a b 1.0))
                  ((da+ db+) (d+ a b 1.0)))
      (check-signatures-not-equal? da- da+)
      (check-signatures-not-equal? db- db+)))

  (let-values (((rkt ds) (compile-tensor (get-test-program 'extract-ds-once-tref))))
    (check-pred
     (λ (ds)
       (eqv? (vector-length ds) 2))
     ds
     (string-append "Tensors and tref indices occurring multiple times in"
                    " source AST but referring to the same tensor AST node must"
                    " be added to the data segment only once.")))
  (let-values (((rkt ds) (compile-tensor (get-test-program 'extract-ds-once-trefs))))
    (check-pred
     (λ (ds)
       (eqv? (vector-length ds) 2))
     ds
     (string-append "Tensors and trefs index lists occurring multiple times in"
                    " source AST but pointing to the same tensor AST node must"
                    " be added to the data segment only once.")))
  )
