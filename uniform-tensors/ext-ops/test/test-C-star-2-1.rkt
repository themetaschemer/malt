(module+ test
  (require rackunit)
  (require (only-in "../tensors.rkt" tensor))

  (let ((a (tensor (tensor 3 4 5 6)
                   (tensor 7 8 9 10)))
        (b (tensor 2 3 4 5)))
    (check-ρ-∇ (d*-2-1 a b)
      (tensor (tensor 6 12 20 30) (tensor 14 24 36 50))
      (list (tensor (tensor 2.0 3.0 4.0 5.0) (tensor 2.0 3.0 4.0 5.0))
            (tensor 10.0 12.0 14.0 16.0))))

  (let ((a (tensor (tensor 3 4 5 6)
                   (tensor 7 8 9 10)))
        (b (tensor (tensor 2 3 4 5)
                   (tensor 12 13 14 15))))

    (check-ρ-∇ (d*-2-1 a b)
      (tensor (tensor (tensor 6 12 20 30) (tensor 14 24 36 50))
              (tensor (tensor 36 52 70 90) (tensor 84 104 126 150)))
      (list (tensor (tensor 14.0 16.0 18.0 20.0)
                    (tensor 14.0 16.0 18.0 20.0))
            (tensor (tensor 10.0 12.0 14.0 16.0)
                    (tensor 10.0 12.0 14.0 16.0))))))
