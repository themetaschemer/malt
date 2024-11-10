(module+ test
  (require (only-in "../tensors.rkt" tensor))
  (require "../../utils.rkt")

  (let ((y (tensor 0.0 0.0 1.0 0.0)))
    (unsafe-test
     (check-ρ-∇ (d-argmax y) 2.0
                (list (tensor 0.0 0.0 0.0 0.0)))))

  (let ((y (tensor -10 -3 -2 -5)))
    (unsafe-test
     (check-ρ-∇ (d-argmax y) 2.0
                (list (tensor 0.0 0.0 0.0 0.0)))))

  (let ((y (tensor (tensor 0.0 0.0 1.0 0.0)
                   (tensor 0.0 1.0 0.0 0.0)
                   (tensor 1.0 0.0 0.0 0.0)
                   (tensor 0.0 0.0 0.0 1.0))))
    (unsafe-test
     (check-ρ-∇ (d-argmax y) (tensor 2.0 1.0 0.0 3.0)
                (list
                 (tensor (tensor 0.0 0.0 0.0 0.0)
                         (tensor 0.0 0.0 0.0 0.0)
                         (tensor 0.0 0.0 0.0 0.0)
                         (tensor 0.0 0.0 0.0 0.0)))))))
