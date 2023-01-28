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

  (check-equal?
   (summed-over-filter (tensor (tensor 1 2)
                               (tensor 3 4)
                               (tensor 5 6))
                       (tensor 3 4)
                       2.0
                       0
                       6
                       3
                       2
                       0.0)
   (+-ρ (+-ρ (tensor 2.0 4.0) (tensor 6.0 8.0)) (tensor 10.0 12.0)))

  (check-equal?
   (summed-over-bank bank
                     (tensor 3 4)
                     (tensor 2.0 3.0 4.0 5.0)
                     1
                     6
                     3
                     1
                     3
                     0.0)
   (+-ρ (+-ρ(+-ρ (+-ρ (+-ρ (tensor 2.0 4.0) (tensor 6.0 8.0)) (tensor 10.0 12.0))
              (+-ρ (+-ρ (tensor 21.0 24.0) (tensor 27.0 30.0)) (tensor 33.0 36.0)))
          (+-ρ (+-ρ (tensor 52.0 56.0) (tensor 60.0 64.0)) (tensor 68.0 72.0)))
       (+-ρ (+-ρ (tensor 95.0 100.0) (tensor 105.0 110.0)) (tensor 115.0 120.0))))

  (check-equal?
   (sum-over-bank bank
                  (tensor 3 4)
                  (tensor 2.0 3.0 4.0 5.0)
                  1
                  6
                  3
                  1)
   (+-ρ (+-ρ(+-ρ (+-ρ (+-ρ (tensor 2.0 4.0) (tensor 6.0 8.0)) (tensor 10.0 12.0))
              (+-ρ (+-ρ (tensor 21.0 24.0) (tensor 27.0 30.0)) (tensor 33.0 36.0)))
          (+-ρ (+-ρ (tensor 52.0 56.0) (tensor 60.0 64.0)) (tensor 68.0 72.0)))
       (+-ρ (+-ρ (tensor 95.0 100.0) (tensor 105.0 110.0)) (tensor 115.0 120.0))))


  (define our-z
    (tensor (tensor 1.0 2.0 3.0 4.0)
            (tensor 5.0 6.0 7.0 8.0)
            (tensor 9.0 10.0 11.0 12.0)
            (tensor 13.0 14.0 15.0 16.0)
            (tensor 17.0 18.0 19.0 20.0)
            (tensor 21.0 22.0 23.0 24.0)))

  (check-equal?
   (derivative-of-signal bank signal our-z)
   (tensor (tensor 280.0 300.0)
           (tensor 1026.0 1104.0)
           (tensor 1602.0 1728.0)
           (tensor 2178.0 2352.0)
           (tensor 2754.0 2976.0)
           (tensor 2400.0 2580.0)))

  (check-equal?
   (summed-over-segments (tensor 19.0 20.0)
                         signal
                         our-z
                         1
                         0
                         5
                         0.0)
   (tensor 572.0 644.0))

  (check-equal?
   (sum-over-segments (tensor 19.0 20.0) signal our-z 1 0)
   (tensor 572.0 644.0))

  (check-equal?
   (sum-over-filter (tref bank 0) signal our-z 0)
   (tensor (tensor 405.0 470.0)
           (tensor 536.0 602.0)
           (tensor 395.0 440.0)))

  (check-equal?
   (derivative-of-bank bank signal our-z)
   (tensor (tensor (tensor 405.0 470.0) (tensor 536.0 602.0) (tensor 395.0 440.0))
           (tensor (tensor 430.0 500.0) (tensor 572.0 644.0) (tensor 430.0 480.0))
           (tensor (tensor 455.0 530.0) (tensor 608.0 686.0) (tensor 465.0 520.0))
           (tensor (tensor 480.0 560.0) (tensor 644.0 728.0) (tensor 500.0 560.0))))

  (let-values (((dbank dsignal) (correlate-3-2-∇ bank signal our-z)))
    (check-equal? dbank
                  (tensor (tensor (tensor 405.0 470.0) (tensor 536.0 602.0) (tensor 395.0 440.0))
                          (tensor (tensor 430.0 500.0) (tensor 572.0 644.0) (tensor 430.0 480.0))
                          (tensor (tensor 455.0 530.0) (tensor 608.0 686.0) (tensor 465.0 520.0))
                          (tensor (tensor 480.0 560.0) (tensor 644.0 728.0) (tensor 500.0 560.0))))
    (check-equal? dsignal
                  (tensor (tensor 280.0 300.0)
                          (tensor 1026.0 1104.0)
                          (tensor 1602.0 1728.0)
                          (tensor 2178.0 2352.0)
                          (tensor 2754.0 2976.0)
                          (tensor 2400.0 2580.0))))

  (check-equal? (dotted-product
                 (tensor 1 2 3 4)
                 (tensor 1 2 3 4)
                 3
                 0.0)
                30.0)

  (check-equal? (dot-product
                 (tensor 1 2 3 4)
                 (tensor 1 2 3 4))
                30.0)

  (check-equal? (sum-dp (tref bank 0) signal -1 2 0.0)
                50.0)

  (check-equal? (correlate-overlap (tref bank 0) signal 0)
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
