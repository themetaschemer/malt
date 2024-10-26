(module+ test
  (require rackunit)
  (require "B-test-programs.rkt")
  (require (prefix-in acc: "../../accelerated-tensors/tensors.rkt"))

  (for (((test-name test-data) (in-hash test-programs)))
     (match-define (test-program-data th res) test-data)
     (match res
       ((eval-res-1 res)
        (let* ((tp (th))
               (interped (interp-tensor tp)))
          (acc:check-tensor-equal?
           interped res
           (format "Expected result doesn't match in test case ~a"
                   test-name))
          (check-equal? (tpromise-shape tp) (acc:shape interped))))
       ((eval-res-2 res1 res2)
        (let*-values (((tp1 tp2) (th))
                      ((interped1) (interp-tensor tp1))
                      ((interped2) (interp-tensor tp2)))
          (acc:check-tensor-equal?
           interped1 res1
           (format "Expected first result doesn't match in test case ~a"
                   test-name))
          (check-equal? (tpromise-shape tp1) (acc:shape interped1))
          (acc:check-tensor-equal?
           interped2 res2
           (format "Expected second result doesn't match in test case ~a"
                   test-name))
          (check-equal? (tpromise-shape tp2) (acc:shape interped2))))))

)
