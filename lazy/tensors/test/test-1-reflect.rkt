(module+ test
  (require rackunit)
  (require ffi/vector)
  (require "0-lazy.rkt")
  (require "B-test-programs.rkt")

  (define evaluated-tpromise?
    (λ (tp)
      (or (tpromise-flat? tp)
          (number? (tpromise-tensor tp)))))

  (for (((test-name test-data) (in-hash test-programs)))
     (match-define (test-program-data th res) test-data)
     (match res
       ((eval-res-1 res)
        (let* ((tp (th))
               (forced (↓ tp)))
          (acc:check-tensor-equal?
           forced res
           (format "Expected result doesn't match in test case ~a"
                   test-name))
          (check-pred evaluated-tpromise? tp)
          (check-equal? (tpromise-shape tp) (acc:shape forced))))
       ((eval-res-2 res1 res2)
        (let*-values (((tp1 tp2) (th))
                      ((forced1) (↓ tp1))
                      ((forced2) (↓ tp2)))
          (acc:check-tensor-equal?
           forced1 res1
           (format "Expected first result doesn't match in test case ~a"
                   test-name))
          (check-pred evaluated-tpromise? tp1)
          (check-equal? (tpromise-shape tp1) (acc:shape forced1))
          (acc:check-tensor-equal?
           forced2 res2
           (format "Expected second result doesn't match in test case ~a"
                   test-name))
          (check-pred evaluated-tpromise? tp2)
          (check-equal? (tpromise-shape tp2) (acc:shape forced2))))))


  (define test-tensor-r1-0 (get-test-program 'tensor-r1-0))
  (check-false (acc:flat? (tpromise-tensor test-tensor-r1-0)))
  (check-true (acc:flat? (car (unbox (tpromise-dst test-tensor-r1-0)))))
  (check-exn exn:fail? (λ () (tensor test-tensor-r1-0 4)))
  (check-exn exn:fail? (λ () (tensor 4 test-tensor-r1-0)))

  (define test-tcomp-tref (get-test-program 'tcomp-tref))
  (check-exn exn:fail? (λ () (tref test-tensor-r1-0 5)))

  (define test-nested-tensor (get-test-program 'tensor-r2-0))
  (check-exn exn:fail? (λ () (tref (tref test-nested-tensor 2) 0)))
  (check-exn exn:fail? (λ () (tref test-nested-tensor 2)))
  (check-exn exn:fail? (λ () (tensor test-nested-tensor test-nested-tensor test-tensor-r1-0)))

  (check-equal? (tlen test-tensor-r1-0) 3)
  (check-equal? (tlen test-nested-tensor) 2)

  (define test-nested-list->tensor
    (get-test-program 'tcomp-nested-list->tensor))
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
  (acc:check-tensor-equal? (↓ test-tcomp-partial-eval)
                            (↓ (tensor 1 2 3)))

  (define test-id-scalar (get-test-program 'id-scalar))
  (define test-force-scalar
    (+-ρ test-id-scalar
         (get-test-program 'sum-nested)))
  (void (↓ test-id-scalar))
  (acc:check-tensor-equal? (↓ test-force-scalar)
                            (↓ (tensor 19 21 20)))

  (define test-force-subexpr
    (+-ρ (get-test-program 'id-scalar)
         (get-test-program 'sum-nested)))
  (define test-force-mutate
    (+-ρ test-force-subexpr
         (+-ρ (get-test-program 'sum-nested)
              (get-test-program 'sum-nested))))
  (void (↓ test-force-subexpr))
  (acc:check-tensor-equal? (↓ test-force-mutate)
                            (↓ (tensor 27 33 30)))

  (define test-tp-r1 (tensor -1 -2 -3))
  (define test-force-supexpr (abs-ρ test-tp-r1))
  (void (↓ test-force-supexpr))
  (acc:check-tensor-equal? (↓ test-tp-r1)
                            (↓ (tensor -1 -2 -3)))

  (define test-trefs (get-test-program 'tcomp-trefs))
  (check-true (tcomp? (tpromise-tensor test-trefs)))
  (check-exn exn:fail? (λ () (trefs test-nested-tensor '(0 4))))

  (define test-reshape (get-test-program 'tcomp-reshape))
  (check-exn exn:fail? (λ () (reshape '(4 5) test-reshape)))

  (check-pred
   (λ (fs) (andmap (λ (e) (integer? (sqrt e))) fs))
   (f32vector->list (acc:flat-store (↓ test-build-random)))
   "Side-effect of generating random tensor must only be run once")

  (acc:check-tensor-equal? (↓ (get-test-program 'multi-built-tensor))
                            (eval-res-1-res (get-test-eval-res 'multi-built-tensor)))
)
