(module+ test
  (require rackunit)
  (require "C-star-2-1.ss")
  (require (only-in "../tensors.rkt" tensor))
  (require (only-in "A-scalar-ops.ss" d-sqr d* d-))

  (let ((a (tensor 3 4 5)))
    (check-ρ-∇ (sum-1 a) 12
               (list (tensor 1.0 1.0 1.0)))
    (check-ρ-∇ (d-sum a) 12
               (list (tensor 1.0 1.0 1.0))))

  (let ((a (tensor (tensor 3 4 5)
                   (tensor 6 7 8))))
    (check-dual-equal? (d-sum a) (tensor 12 21))
    (check-dual-equal? ((∇¹ (λ (b) (d-sum (d* b b))))  a)
                       (list (tensor (tensor 6.0 8.0 10.0)
                                     (tensor 12.0 14.0 16.0))))
    (check-ρ-∇ (d-sum-cols a) (tensor 9 11 13)
               (list (tensor (tensor 1 1 1)
                             (tensor 1 1 1)))))

  (let ((a (tensor
            (tensor (tensor (tensor 3 4 5) (tensor 6 7 8))
                    (tensor (tensor 8 7 6) (tensor 5 4 3)))
            (tensor (tensor (tensor 1 2 3) (tensor 6 5 4))
                    (tensor (tensor 7 8 9) (tensor 9 8 7))))))
    (check-ρ-∇ (d-sum-cols a)
               (tensor
                (tensor (tensor 9 11 13)
                        (tensor 13 11 9))
                (tensor (tensor 7 7 7)
                        (tensor 16 16 16)))
               (list (tensor
                      (tensor (tensor (tensor 1 1 1) (tensor 1 1 1))
                              (tensor (tensor 1 1 1) (tensor 1 1 1)))
                      (tensor (tensor (tensor 1 1 1) (tensor 1 1 1))
                              (tensor (tensor 1 1 1) (tensor 1 1 1)))))))

  (define dot-product
    (λ (a b)
      (d-sum (d*-2-1 a b))))

  (define sse
    (λ (a b)
      (d-sum (d-sqr (d- a b)))))

  (let ((a (tensor (tensor 3 4 5 6)
                   (tensor 7 8 9 10)))
        (b (tensor 2 3 4 5)))

    (check-ρ-∇ (sum-1 b) 14
               (list (tensor 1.0 1.0 1.0 1.0)))

    (check-ρ-∇ (dot-product a b)
               (tensor 68 124)
               (list (tensor (tensor 2.0 3.0 4.0 5.0)
                             (tensor 2.0 3.0 4.0 5.0))
                     (tensor 10.0 12.0 14.0 16.0)))

    (check-ρ-∇ (sse a b)
               (tensor 4 100)
               (list (tensor (tensor 2.0 2.0 2.0 2.0)
                             (tensor 10.0 10.0 10.0 10.0))
                     (tensor -12.0 -12.0 -12.0 -12.0))))

  (let ((a (tensor (tensor 3 4 5 6)
                   (tensor 7 8 9 10)))
        (b (tensor (tensor 2 3 4 5)
                   (tensor 12 13 14 15))))

    (check-ρ-∇ (dot-product a b)
               (tensor (tensor 68 124)
                       (tensor 248 464))
               (list (tensor (tensor 14.0 16.0 18.0 20.0)
                             (tensor 14.0 16.0 18.0 20.0))
                     (tensor (tensor 10.0 12.0 14.0 16.0)
                             (tensor 10.0 12.0 14.0 16.0))))))
