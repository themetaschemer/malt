(module+ test
  (require rackunit)
  (require (only-in "../tensors.rkt" tensor ext2-∇ check-tensor-equal?))

  ;; for testing b = 4
  ;;             m = 3
  ;;             d = 2

  ;; signal length n = 6

  ;;      (1 2) (3 4) (5 6) (7 8) (9 10) (11 12)
  ;;      (1  2)  (3  4)  (5  6)
  ;;      (7  8)  (9  10) (11 12)
  ;;      (13 14) (15 16) (17 18)
  ;;      (19 20) (21 22) (23 24)

  ;; Signal is (n d)
  (define signal (tensor (tensor 1 2)
                         (tensor 3 4)
                         (tensor 5 6)
                         (tensor 7 8)
                         (tensor 9 10)
                         (tensor 11 12)))

  (define bank (tensor (tensor
                        (tensor 1 2)
                        (tensor 3 4)
                        (tensor 5 6))
                       (tensor
                        (tensor 7 8)
                        (tensor 9 10)
                        (tensor 11 12))
                       (tensor
                        (tensor 13 14)
                        (tensor 15 16)
                        (tensor 17 18))
                       (tensor
                        (tensor 19 20)
                        (tensor 21 22)
                        (tensor 23 24))))

  (define corr-ρ
    (ext2-ρ (correlate-3-1-ρ 12 6 2) 3 1 correlate-shape))

  (define corr-∇
    (ext2-∇ (correlate-3-1-∇ 12 6 2) 3 1 correlate-shape))

  (check-tensor-equal? (corr-ρ bank signal)
                       ;; Should be of size nb
                       (tensor (tensor 50.0 110.0 170.0 230.0)
                               (tensor 91.0 217.0 343.0 469.0)
                               (tensor 133.0 331.0 529.0 727.0)
                               (tensor 175.0 445.0 715.0 985.0)
                               (tensor 217.0 559.0 901.0 1243.0)
                               (tensor 110.0 362.0 614.0 866.0)))

  (let-values (((filter-∇ signal-∇)
                (corr-∇ bank signal (tensor (tensor 1.0 1.0 1.0 1.0)
                                            (tensor 1.0 1.0 1.0 1.0)
                                            (tensor 1.0 1.0 1.0 1.0)
                                            (tensor 1.0 1.0 1.0 1.0)
                                            (tensor 1.0 1.0 1.0 1.0)
                                            (tensor 1.0 1.0 1.0 1.0)))))
    (check-tensor-equal? filter-∇
                         (tensor
                          (tensor (tensor 25.0 30.0)
                                  (tensor 36.0 42.0)
                                  (tensor 35.0 40.0))
                          (tensor (tensor 25.0 30.0)
                                  (tensor 36.0 42.0)
                                  (tensor 35.0 40.0))
                          (tensor (tensor 25.0 30.0)
                                  (tensor 36.0 42.0)
                                  (tensor 35.0 40.0))
                          (tensor (tensor 25.0 30.0)
                                  (tensor 36.0 42.0)
                                  (tensor 35.0 40.0))))
    (check-tensor-equal? signal-∇
                         ;; Should be of size nb
                         (tensor (tensor 88.0 96.0)
                                 (tensor 144.0 156.0)
                                 (tensor 144.0 156.0)
                                 (tensor 144.0 156.0)
                                 (tensor 144.0 156.0)
                                 (tensor 104.0 112.0))))

  (check-dual-equal? (d-correlate bank signal)
                     ;; Should be of size nb
                     (tensor (tensor 50.0 110.0 170.0 230.0)
                             (tensor 91.0 217.0 343.0 469.0)
                             (tensor 133.0 331.0 529.0 727.0)
                             (tensor 175.0 445.0 715.0 985.0)
                             (tensor 217.0 559.0 901.0 1243.0)
                             (tensor 110.0 362.0 614.0 866.0)))

  (let ((gs ((∇¹ d-correlate) bank signal)))
    (check-dual-equal? (car gs)
                       (tensor
                        (tensor (tensor 25.0 30.0)
                                (tensor 36.0 42.0)
                                (tensor 35.0 40.0))
                        (tensor (tensor 25.0 30.0)
                                (tensor 36.0 42.0)
                                (tensor 35.0 40.0))
                        (tensor (tensor 25.0 30.0)
                                (tensor 36.0 42.0)
                                (tensor 35.0 40.0))
                        (tensor (tensor 25.0 30.0)
                                (tensor 36.0 42.0)
                                (tensor 35.0 40.0))))
    (check-dual-equal? (cadr gs)
                       ;; Should be of size nb
                       (tensor (tensor 88.0 96.0)
                               (tensor 144.0 156.0)
                               (tensor 144.0 156.0)
                               (tensor 144.0 156.0)
                               (tensor 144.0 156.0)
                               (tensor 104.0 112.0)))))
