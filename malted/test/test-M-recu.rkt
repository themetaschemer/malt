(module+ test
  (require rackunit)

  (define s2d1
    (tensor (tensor 3.0)
            (tensor 4.0)
            (tensor 5.0)
            (tensor 6.0)
            (tensor 7.0)
            (tensor 8.0)))

  (define b2f3d1
    (tensor
     (tensor (tensor 3.0)
             (tensor 4.0)
             (tensor 5.0))
     (tensor (tensor 3.0)
             (tensor 4.0)
             (tensor 5.0))))

  (define b2f3d2
    (tensor
     (tensor (tensor 3.0 3.5)
             (tensor 4.0 4.5)
             (tensor 5.0 5.5))
     (tensor (tensor 3.0 3.5)
             (tensor 4.0 4.5)
             (tensor 5.0 5.5))))

  (define positive-biases (tensor 3.0 4.0))

  (check-dual-equal? ((corr s2d1) (list b2f3d1 positive-biases))
                     (tensor (tensor 35.0 36.0)
                             (tensor 53.0 54.0)
                             (tensor 65.0 66.0)
                             (tensor 77.0 78.0)
                             (tensor 89.0 90.0)
                             (tensor 56.0 57.0)))

  (check-dual-equal? ((recu s2d1) (list b2f3d1 positive-biases))
                     (tensor (tensor 35.0 36.0)
                             (tensor 53.0 54.0)
                             (tensor 65.0 66.0)
                             (tensor 77.0 78.0)
                             (tensor 89.0 90.0)
                             (tensor 56.0 57.0)))

  (define negative-biases (tensor -70.0 -60.0))

  (check-dual-equal? ((corr s2d1) (list b2f3d1 negative-biases))
                     (tensor (tensor -38.0 -28.0)
                             (tensor -20.0 -10.0)
                             (tensor -8.0 2.0)
                             (tensor 4.0 14.0)
                             (tensor 16.0 26.0)
                             (tensor -17.0 -7.0)))

  (check-dual-equal? ((recu s2d1) (list b2f3d1 negative-biases))
                     (tensor (tensor 0.0 0.0)
                             (tensor 0.0 0.0)
                             (tensor 0.0 2.0)
                             (tensor 4.0 14.0)
                             (tensor 16.0 26.0)
                             (tensor 0.0 0.0)))

  (check-dual-equal? (((k-recu 2) s2d1) (list b2f3d1 negative-biases
                                              b2f3d2 positive-biases))
                     (tensor (tensor 3.0 4.0)
                             (tensor 14.0 15.0)
                             (tensor 109.0 110.0)
                             (tensor 312.0 313.0)
                             (tensor 245.0 246.0)
                             (tensor 142.0 143.0))))
