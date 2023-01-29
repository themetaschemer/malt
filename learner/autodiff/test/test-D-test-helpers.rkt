(module+ test
  (require rackunit)

  (check-true ((equal-within-tolerance?) 1.00001 1.0001))
  (check-true ((equal-within-tolerance?) 1.0002 1.0001))
  (check-false ((equal-within-tolerance?) 1.0003 1.0001))

  (define t0
    (vector (vector (vector 0.0 2.0 4.0 6.0) (vector 8.0 10.0 12.0 14.0) (vector 16.0 18.0 20.0 22.0))
            (vector (vector 24.0 26.0 28.0 30.0) (vector 32.0 34.0 36.0 38.0) (vector 40.0 42.0 44.0 46.0))))

  (define t1
    (vector (vector (vector 0.0 2.00001 4.00001 6.00001)
                    (vector 8.00001 10.00001 12.00001 14.00001)
                    (vector 16.00001 18.00001 20.00001 22.00001))
            (vector (vector 24.00001 26.00001 28.00001 30.00001)
                    (vector 32.00001 34.00001 36.00001 38.00001)
                    (vector 40.00001 42.00001 44.00001 46.00001))))

  (define t2
    (vector (vector (vector (vector 0.0 2.00001 4.00001 6.00001)
                            (vector 8.00001 10.00001 12.00001 14.00001)
                            (vector 16.00001 18.00001 20.00001 22.00001))
                    (vector (vector 24.00001 26.00001 28.00001 30.00001)
                            (vector 32.00001 34.00001 36.00001 38.00001)
                            (vector 40.00001 42.00001 44.00001 46.00001)))))

  (check-true (equal-wt? t0 t1))

  (check-false (equal-wt? t0 t2))

  (check-true (tensor-equal? t0 t1))

  (check-false (tensor-equal? t0 t2))

  (check-tensor-equal? t0 t1))
