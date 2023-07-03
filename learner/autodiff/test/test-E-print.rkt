(module+ test
  (require rackunit)
  (require "../tensors.rkt")

  (define long-tensor
    (tensor 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

  (define dualized-long-tensor
    (tensor (dual 1 end-of-chain) (dual 2 end-of-chain)
            (dual 3 end-of-chain) (dual 4 end-of-chain)
            (dual 5 end-of-chain) (dual 6 end-of-chain)
            (dual 7 end-of-chain) (dual 8 end-of-chain)
            (dual 9 end-of-chain) (dual 10 end-of-chain)
            (dual 11 end-of-chain) (dual 12 end-of-chain)
            (dual 13 end-of-chain) (dual 14 end-of-chain)
            (dual 15 end-of-chain)))

  (define deep-tensor
    (tensor long-tensor long-tensor long-tensor long-tensor long-tensor
            long-tensor long-tensor long-tensor long-tensor long-tensor
            long-tensor long-tensor long-tensor long-tensor long-tensor))

  (define deeper-tensor
    (tensor deep-tensor deep-tensor deep-tensor deep-tensor deep-tensor
            deep-tensor deep-tensor deep-tensor deep-tensor deep-tensor
            deep-tensor deep-tensor deep-tensor deep-tensor deep-tensor))

  (check-equal? (call/cc (λ (return) (tensor-like-vector? long-tensor return))) '(15))
  (check-equal? (call/cc (λ (return) (tensor-like-vector? deep-tensor return))) '(15 15))
  (check-equal? (call/cc (λ (return) (tensor-like-vector? deeper-tensor return))) '(15 15 15))
  (check-equal? (call/cc (λ (return) (tensor-like-vector? dualized-long-tensor return))) '(15))

  (check-equal? (vector->tensor-list long-tensor 3) (fake-tensor '(1 2 3 ...)))
  (check-equal? (vector->tensor-list deep-tensor 3)
                (fake-tensor `(,(fake-tensor '(1 2 3 ...)) ,(fake-tensor '(1 2 3 ...)) ,(fake-tensor '(1 2 3 ...)) ...)))

  (check-equal? (vector->tensor-list deeper-tensor 3)
                (fake-tensor (list (fake-tensor (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                                   (fake-tensor (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                                   (fake-tensor (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                                   '...)))
  (check-equal? (vector->tensor-list dualized-long-tensor 3) (fake-tensor '(1 2 3 ...)))

  (parameterize ((max-tensor-print-length 3))
    (check-equal? (make-printable (list long-tensor dualized-long-tensor deeper-tensor))
                  (list (fake-tensor '(1 2 3 ...))
                        (fake-tensor '(1 2 3 ...))
                        (fake-tensor
                         (list (fake-tensor (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                               (fake-tensor (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                               (fake-tensor (list (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) (fake-tensor '(1 2 3 ...)) '...))
                               '...))))))
