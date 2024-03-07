(module+ test
  (require (only-in "../tensors.rkt" tensor))

  (let ((y (tensor 0.0 0.0 1.0 0.0)))
    (check-ρ-∇ (d-argmax y) 2.0
               (list (tensor 0.0 0.0 0.0 0.0))))

  (let ((y (tensor (tensor 0.0 0.0 1.0 0.0)
                   (tensor 0.0 1.0 0.0 0.0)
                   (tensor 1.0 0.0 0.0 0.0)
                   (tensor 0.0 0.0 0.0 1.0))))
    (check-ρ-∇ (d-argmax y) (tensor 2.0 1.0 0.0 3.0)
               (list
                (tensor (tensor 0.0 0.0 0.0 0.0)
                        (tensor 0.0 0.0 0.0 0.0)
                        (tensor 0.0 0.0 0.0 0.0)
                        (tensor 0.0 0.0 0.0 0.0))))))
