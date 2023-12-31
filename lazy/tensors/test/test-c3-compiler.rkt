(module+ test
  (require rackunit)
  (require "B-test-programs.rkt")
  (require "0-lazy.rkt")

  (define current-test-program-name (make-parameter #f))
  (define-check (check-compiler-invariants tp)
    (let-values (((instrs ds) (compile-tensor tp)))
      (with-check-info
          (('data-segment ds)
           ('instrs instrs))
        (define test-name-string
          (cond
            ((current-test-program-name) (format "In test case: ~a"
                                                 (current-test-program-name)))
            (else "")))
        'ok
        ;;TODO: Add a check to ensure the number of tcomp-ds-ref occurring in
        ;;the input equals the size of the data segment
        #;
        (for ((name/flat ds))
          (unless (and (flat:flat? (cdr name/flat))
                       (not (null? (flat:flat-shape (cdr name/flat)))))
            (fail-check (format (string-append "Value associated with the variable"
                                               " ~a should be a flat tensor. "
                                               "Associated value found: ~a")
                                (car name/flat) (cdr name/flat))))))))

  (for (((test-name test-data) (in-hash test-programs)))
     (match-define (test-program-data th res) test-data)
    (parameterize ((current-test-program-name test-name))
      (match res
        ((eval-res-1 res)
         (let* ((tp (th)))
           (check-compiler-invariants tp)))
        ((eval-res-2 res1 res2)
         (let*-values (((tp1 tp2) (th)))
           (check-compiler-invariants tp1)
           (check-compiler-invariants tp2))))))
  (define-check (check-signatures-equal? t1 t2)
    (let ((sig1 (tpromise-sign t1))
          (sig2 (tpromise-sign t2)))
      (with-check-info
          (('signature-1 sig1)
           ('signature-2 sig2))
        (unless (equal? sig1 sig2)
          (fail-check "signature mismatch")))))

  (define-check (check-signatures-not-equal? t1 t2)
    (let ((sig1 (tpromise-sign t1))
          (sig2 (tpromise-sign t2)))
      (with-check-info
          (('signature-1 sig1)
           ('signature-2 sig2))
      (when (equal? sig1 sig2)
            (fail-check "signatures musn't match")))))

  (define test-tensor-r1-1 (get-test-program 'tensor-r1-1))
  (define test-tcomp-tref (get-test-program 'tcomp-tref))
  (check-signatures-equal? test-tcomp-tref
                           (make-tref-test-program test-tensor-r1-1))
  (check-signatures-not-equal? test-tcomp-tref
                               (make-list->tensor-test-program `(,test-tensor-r1-1)))

  (define tensor-r1 (get-test-program 'tensor-r1-0))
  (check-signatures-equal? (*-ρ 2 tensor-r1) (*-ρ 3 tensor-r1))

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
       (eqv? (set-count (list->seteq (vector->list ds))) 2))
     ds
     (string-append "eq? equivalent flat tensors and tref indices"
                    " used to construct the source AST must"
                    " be eq? equivalent in the data segment as well.")))
  (let-values (((rkt ds) (compile-tensor (get-test-program 'extract-ds-once-trefs))))
    (check-pred
     (λ (ds)
       (eqv? (set-count (list->seteq (vector->list ds))) 2))
     ds
     (string-append "eq? equivalent flat tensors and trefs index lists"
                    " used to construct the source AST must"
                    " be eq? equivalent in the data segment as well.")))

  (define count-tcomp-var
    (λ (tp)
      (ctv-tcomp (tpromise-tensor tp))))

  (define ctv-tcomp
    (λ (tc)
      (match tc
      ((? number?) 0)
      [(tcomp-list->tensor lst)
       (for/sum
        ((l lst))
         (cond
           ((tpromise? l) (count-tcomp-var l))
           ((number? l) 0)
           (else (error 'cdsr-list->tensor "Unexpected: ~a" l))))]
      [(tcomp-tref tp _) (count-tcomp-var tp)]
      [(tcomp-trefs tp _) (count-tcomp-var tp)]
      [(tcomp-ext2-∇ fᵈ sign r0 r1 shape-fn tp-t0 tp-t1 tp-z
                     out-ref0
                     out-ref1 i)
       (let ((c0 (count-tcomp-var tp-t0))
             (c1 (count-tcomp-var tp-t1))
             (cz (count-tcomp-var tp-z)))
         (+ c0 c1 cz))]
      [(tcomp-ext1-∇ tp zp f sign m shape-fn)
       (let ((ct (count-tcomp-var tp))
             (cz (count-tcomp-var zp)))
         (+ ct cz))]
      [(tcomp-ext2-ρ-scalar f sign tp-t tp-u)
       (let ((ct (count-tcomp-var tp-t))
             (cu (count-tcomp-var tp-u)))
         (+ ct cu))]
      [(tcomp-ext2-ρ tp-t tp-u f sign m n shape-fn)
       (let ((ct (count-tcomp-var tp-t))
             (cu (count-tcomp-var tp-u)))
         (+ ct cu))]
      [(tcomp-ext1-ρ-scalar f sign tp) (count-tcomp-var tp)]
      [(tcomp-ext1-ρ f sign m shape-fn tp) (count-tcomp-var tp)]
      [(tcomp-reshape s tp) (count-tcomp-var tp)]
      [(tcomp-ds-ref i) 0]
      [(tcomp-let lhs rhs body)
       (let ((cr (count-tcomp-var rhs))
             (cb (count-tcomp-var body)))
         (+ cr cb))]
      [(tcomp-var name) 1])))

  (define get-common-subexprs
    (λ (tp)
      (let ((instrs (generate-ds-refs tp)))
        (extract-common-subexpressions instrs (count-references instrs)))))

  (check-equal?
   (count-tcomp-var (get-common-subexprs (get-test-program 'common-subexpression)))
   2)
  (check-equal?
   (count-tcomp-var
    (get-common-subexprs (get-test-program 'nested-common-subexpression)))
   2)
)
