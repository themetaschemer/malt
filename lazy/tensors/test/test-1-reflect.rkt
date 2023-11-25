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

  (for (((test-name test-data) (in-hash test-programs)))
     (match-define (test-program-data th res) test-data)
     (match res
       ((eval-res-1 res)
        (let* ((tp (th))
               (forced (↓ tp)))
          (flat:check-tensor-equal?
           forced res
           (format "Expected result doesn't match in test case ~a"
                   test-name))
          (check-false (tcomp? (tpromise-tensor tp)))
          (check-equal? (tpromise-shape tp) (flat:shape forced))))
       ((eval-res-2 res1 res2)
        (let*-values (((tp1 tp2) (th))
                      ((forced1) (↓ tp1))
                      ((forced2) (↓ tp2)))
          (flat:check-tensor-equal?
           forced1 res1
           (format "Expected first result doesn't match in test case ~a"
                   test-name))
          (check-false (tcomp? (tpromise-tensor tp1)))
          (check-equal? (tpromise-shape tp1) (flat:shape forced1))
          (flat:check-tensor-equal?
           forced2 res2
           (format "Expected second result doesn't match in test case ~a"
                   test-name))
          (check-false (tcomp? (tpromise-tensor tp2)))
          (check-equal? (tpromise-shape tp2) (flat:shape forced2))))))


  (define test-tensor-r1-0 (get-test-program 'tensor-r1-0))
  (check-true (flat:flat? (tpromise-tensor test-tensor-r1-0)))
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
  (flat:check-tensor-equal? (↓ test-tcomp-partial-eval)
                            (↓ (tensor 1 2 3)))

  (define test-trefs (get-test-program 'tcomp-trefs))
  (check-true (tcomp? (tpromise-tensor test-trefs)))
  (check-exn exn:fail? (λ () (trefs test-nested-tensor '(0 4))))

  (define test-reshape (get-test-program 'tcomp-reshape))
  (check-exn exn:fail? (λ () (reshape '(4 5) test-reshape)))

  (check-pred
   (λ (fs) (andmap (λ (e) (integer? (sqrt e))) fs))
   (vector->list (flat:flat-store (↓ test-build-random)))
   "Side-effect of generating random tensor must only be run once")

  (flat:check-tensor-equal? (↓ (get-test-program 'multi-built-tensor))
                            (eval-res-1-res (get-test-eval-res 'multi-built-tensor)))
)
