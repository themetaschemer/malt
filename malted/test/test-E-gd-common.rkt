(module+ test
  (require rackunit)
  (require "../impl.rkt")

  (check-dual-equal? (zeroes (tensor 1 2 3))
                (tensor 0.0 0.0 0.0))

  (check-dual-equal? (smooth 0.9 31 -8) 27.1)

  (check-dual-equal? (smooth 0.9 27.1 4) 24.79)

  (with-hypers ((mu 0.5) (beta 0.3))
    (check-dual-equal? (+ mu beta) 0.8)))
