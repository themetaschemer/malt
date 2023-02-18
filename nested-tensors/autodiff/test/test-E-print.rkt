(module+ test
  (require rackunit)
  (require "../tensors.rkt")
  (require "A-autodiff.rkt")

  (define long-tensor
    (tensor 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

  (define dualized-long-tensor
    (dual long-tensor end-of-chain))

  (define deep-tensor
    (tensor long-tensor long-tensor long-tensor long-tensor long-tensor
            long-tensor long-tensor long-tensor long-tensor long-tensor
            long-tensor long-tensor long-tensor long-tensor long-tensor))

  (define deeper-tensor
    (tensor deep-tensor deep-tensor deep-tensor deep-tensor deep-tensor
            deep-tensor deep-tensor deep-tensor deep-tensor deep-tensor
            deep-tensor deep-tensor deep-tensor deep-tensor deep-tensor))

  (define not-a-tensor
    (vector deep-tensor long-tensor))

  (check-equal? (call/cc (λ (return) (tensor-like-vector? long-tensor return))) '(15))
  (check-equal? (call/cc (λ (return) (tensor-like-vector? deep-tensor return))) '(15 15))
  (check-equal? (call/cc (λ (return) (tensor-like-vector? deeper-tensor return))) '(15 15 15))
  (check-false (call/cc (λ (return) (tensor-like-vector? not-a-tensor return))))

  (check-equal? (vector->tensor-list long-tensor 3) (fake-tensor (list 1 2 3 '...)))
  (check-equal? (vector->tensor-list deep-tensor 3)
                (fake-tensor (list (fake-tensor (list 1 2 3 '...))
                                   (fake-tensor (list 1 2 3 '...))
                                   (fake-tensor (list 1 2 3 '...))
                                   '...)))

  (check-equal? (vector->tensor-list deeper-tensor 3)
                (fake-tensor
                  (list
                    (fake-tensor (list (fake-tensor (list 1 2 3 '...))
                                       (fake-tensor (list 1 2 3 '...))
                                       (fake-tensor (list 1 2 3 '...))
                                       '...))
                    (fake-tensor (list (fake-tensor (list 1 2 3 '...))
                                       (fake-tensor (list 1 2 3 '...))
                                       (fake-tensor (list 1 2 3 '...))
                                       '...))
                    (fake-tensor (list (fake-tensor (list 1 2 3 '...))
                                       (fake-tensor (list 1 2 3 '...))
                                       (fake-tensor (list 1 2 3 '...))
                                       '...))
                    '...)))

  (parameterize ((max-tensor-print-length 3))
    (check-equal? (make-printable dualized-long-tensor 3) (fake-tensor '(1 2 3 ...)))

    (check-equal? (make-printable (list long-tensor dualized-long-tensor deeper-tensor))
                  (list (fake-tensor '(1 2 3 ...))
                        (fake-tensor '(1 2 3 ...))
                        (fake-tensor
                         (list
                          (fake-tensor
                           (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                          (fake-tensor
                           (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                          (fake-tensor
                           (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                          '...))))))
