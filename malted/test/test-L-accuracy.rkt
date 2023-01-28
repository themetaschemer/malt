(module+ test
  (require rackunit)

  (define t2 (tensor (tensor 1 2 3 4)
                     (tensor 5 6 7 8)))

  (define a-model
    (λ (t) t))

  (check-dual-equal? (accuracy a-model t2 t2) 1.0))
