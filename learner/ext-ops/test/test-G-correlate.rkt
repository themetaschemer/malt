(module+ test
  (require rackunit)
  (require (only-in "../tensors.rkt" tensor))

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

  ;; for testing b = 4
  ;;             m = 3
  ;;             d = 2

  (check-dual-equal? (dotted-product
                      (tensor 1 2 3 4)
                      (tensor 1 2 3 4)
                      3
                      0.0)
                     30.0)

  (check-dual-equal? (dot-product
                      (tensor 1 2 3 4)
                      (tensor 1 2 3 4))
                     30.0)

  (check-dual-equal? (sum-dp (tref bank 0) signal -1 2 0.0)
                     50.0)

  (check-dual-equal? (correlate-overlap (tref bank 0) signal 0)
                     50.0)

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
