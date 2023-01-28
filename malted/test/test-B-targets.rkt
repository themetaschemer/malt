(module+ test
  (require rackunit)

  (define d1 3.0)
  (define d2 4.0)

  (define r1d1 (tensor 3.0 4.0 5.0))
  (define r1d2 (tensor 7.0 8.0 9.0))

  (define r2d1 (tensor (tensor 3.0 4.0 5.0) (tensor 3.0 4.0 5.0)))
  (define r2d2 (tensor (tensor 7.0 8.0 9.0) (tensor 7.0 8.0 9.0)))

  (define r2d3 (tensor (tensor 3.0 4.0 5.0) (tensor 7.0 8.0 9.0)))
  (define r2d4 (tensor (tensor 7.0 8.0 9.0) (tensor 3.0 4.0 5.0)))

  (define line-theta-1 (list 1.0 2.0))

  (check-ρ-∇ ((line r1d1) line-theta-1)
   (tensor 5.0 6.0 7.0)
   (list '(12.0 3.0)))

  (check-ρ-∇ ((line r1d2) line-theta-1)
   (tensor 9.0 10.0 11.0)
   (list '(24.0 3.0)))

  (check-ρ-∇ ((line r2d1) line-theta-1)
   (tensor (tensor 5.0 6.0 7.0) (tensor 5.0 6.0 7.0))
   (list '(24.0 6.0)))

  (define quad-theta-1 (list 14.0 2.0 8.0))

  (check-ρ-∇ ((quad r1d1) quad-theta-1)
   (tensor 140.0 240.0 368.0)
   (list '(50.0 12.0 3.0)))

  (check-ρ-∇ ((quad r1d2) quad-theta-1)
   (tensor 708.0 920.0 1160.0)
   (list '(194.0 24.0 3.0)))

  (check-ρ-∇ ((quad r2d1) quad-theta-1)
   (tensor (tensor 140.0 240.0 368.0) (tensor 140.0 240.0 368.0))
   (list '(100.0 24.0 6.0)))

  (check-ρ-∇ ((quad r2d2) quad-theta-1)
   (tensor (tensor 708.0 920.0 1160.0) (tensor 708.0 920.0 1160.0))
   (list '(388.0 48.0 6.0)))

  (define plane-theta-1 (list (tensor 9.0 8.0 7.0) 2.0))

  (check-ρ-∇ ((plane r1d1) plane-theta-1)
   96.0
   (list (list (tensor 3.0 4.0 5.0) 1.0)))

  (check-ρ-∇ ((plane r2d1) plane-theta-1)
   (tensor 96.0 96.0)
   (list (list (tensor 6.0 8.0 10.0) 2.0)))

  (check-dual-equal? ((linear (tensor -1 0 1)) (list (tensor (tensor 1 2 3)
                                                             (tensor 1 2 3)) 2))
                     (tensor 4.0 4.0) "linear")

  (check-dual-equal? ((linear (tensor (tensor -1 0 1)
                                      (tensor -1 0 1)))
                      (list (tensor (tensor 1 2 3)
                                    (tensor 1 2 3)) 2))
                     (tensor (tensor 4.0 4.0)
                             (tensor 4.0 4.0)) "linear")

  (check-dual-equal? (∇ (linear (tensor (tensor -1 0 1)
                                         (tensor -1 0 2)))
                        (list (tensor (tensor 1 2 3) (tensor 1 2 4)) 2))
                      (list (tensor (tensor -2.0 0.0 3.0)
                                    (tensor -2.0 0.0 3.0))
                            4.0) "gradient linear")

  (check-dual-equal?
   ((linear (tensor (tensor -1 0 1) (tensor -1 0 1) (tensor -1 0 1)))
    (list (tensor (tensor 1 2 3)
                  (tensor 1 2 3))
          2))
   (tensor (tensor 4.0 4.0) (tensor 4.0 4.0) (tensor 4.0 4.0)) "linear")

  (check-dual-equal? (gradient-of (linear (tensor (tensor -1 0 1) (tensor -1 0 1)))
                                  (list (tensor (tensor 1 2 3)
                                                (tensor 1 2 3)) 2))
                     (list (tensor (tensor -2.0 0.0 2.0) (tensor -2.0 0.0 2.0)) 4.0) "gradient linear"))
