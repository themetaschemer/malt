(module+ test
  (require rackunit)
  (require "B-layer-fns.ss")

  (define r2d1 (tensor (tensor 3.0 4.0 5.0) (tensor 3.0 4.0 5.0)))

  (define plane-theta-0 (list (tensor 0.0 0.0 0.0) 0.0))
  (check-ρ-∇ (((l2-loss plane) r2d1 (tensor 1.0 1.0)) plane-theta-0)
   2.0
   (list (list (tensor -12.0 -16.0 -20.0) -4.0)))

  (define dist1 (tensor (tensor 0.3 0.4 0.3) (tensor 0.1 0.1 0.8)))

  (define ce-theta
    (list (tensor 0.9 0.05 0.05)
          (tensor 0.8 0.1 0.1)))

  (define bad
    (λ (t)
      (λ (theta)
        (+ (* t (ref theta 0))
           (ref theta 1)))))

  (check-ρ-∇ (((cross-entropy-loss bad) dist1 dist1) ce-theta)
   (tensor 0.49221825504118605 0.6033077198687743)
   (list (list (tensor -0.03178270152963002 -0.47619047619047616 -1.7846790890269149)
               (tensor -0.1309111274458329 -1.4285714285714284 -2.774327122153209))))

  (check-ρ-∇ (((kl-loss bad) dist1 dist1) ce-theta)
   (tensor 1.1059011281529316 1.706692900826487)
   (list (list (tensor 1.0000945635137346 0.023289894686568724 -0.5820305479347834)
               (tensor 5.457682729537845 0.8448173598434958 -0.701819651351574)))))
