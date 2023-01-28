(module+ test
  (require rackunit)
  (require "../base.rkt")

  (check-dual-equal? (gradient-of (relu (tensor -1 0 1))
                                  (list (tensor (tensor 1 2 3)
                                                (tensor 1 2 3)) 2))
                     (list (tensor (tensor -1.0 0.0 1.0) (tensor -1.0 0.0 1.0))
                           2.0)
                     "gradient relu positive")

  (check-dual-equal? ((∇¹ (relu (tensor -1 0 1)))
                      (list (tensor (tensor 1 2 -3)
                                    (tensor 1 2 -3))
                            2))
                     (list (list (tensor (tensor 0.0 0.0 0.0)
                                         (tensor 0.0 0.0 0.0))
                                 0.0)))

  (define theta-shapes '((3 2) (3)))

  ;; generated using (map init-shape theta-shapes)
  (define theta
    (list (tensor (tensor 1.5043209265388457 1.5892702938568741)
                  (tensor 0.33592431328374556 0.8653082103400623)
                  (tensor -0.8007586188977664 -1.4723530725283407))
          (tensor 0 0 0)))

  (check-dual-equal? ((relu (tensor (tensor 2 3) (tensor 2 3))) theta)
                     (tensor (tensor 7.7764527346483145
                                     3.267773257587678
                                     -0.0)
                             (tensor 7.7764527346483145
                                     3.267773257587678
                                     -0.0)))

  (check-dual-equal? ((relu (tensor (tensor -1 0 1) (tensor -1 0 1)))

                      (list (tensor (tensor 1 2 3)
                                    (tensor 1 2 3))
                            2))
                     (tensor (tensor 4.0 4.0)
                             (tensor 4.0 4.0)) "relu")

  (check-dual-equal? (gradient-of (relu (tensor (tensor -1 0 1) (tensor -1 0 1)))
                                  (list (tensor (tensor 1 2 3)
                                                (tensor 1 2 3)) 2))
                     (list (tensor (tensor -2.0 0.0 2.0)
                                   (tensor -2.0 0.0 2.0)) 4.0) "gradient relu positive")

  (define test-classifier (k-relu 2))
  (define test-widths '(8 3))
  )
