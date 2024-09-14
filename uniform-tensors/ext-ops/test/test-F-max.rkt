(module+ test
  (require rackunit)
  (require (only-in "../tensors.rkt" tensor))

  (let ((y (tensor (tensor 0.0 0.0 1.0 0.0)
                   (tensor 0.0 1.0 0.0 0.0)
                   (tensor 1.0 0.0 0.0 0.0)
                   (tensor 0.0 0.0 0.0 1.0))))
    (check-ρ-∇ (d-max y) (tensor 1.0 1.0 1.0 1.0)
               (list y))))
