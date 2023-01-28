(module+ test
  (require rackunit)

  (check-true ((equal-within-tolerance?) 1.00001 1.0001))
  (check-true ((equal-within-tolerance?) 1.0002 1.0001))
  (check-false ((equal-within-tolerance?) 1.0003 1.0001))

  (define t0
    (flat '(2 3 4)
          (build-vector 24
            (λ (i)
              (* 2.0 i)))
          0))


  (define t1
    (flat '(2 3 4)
          (build-vector 24
            (λ (i)
              (* 2.000001 i)))
          0))

  (define t2
    (flat '(1 2 3 4)
          (build-vector 24
            (λ (i)
              (* 2.000001 i)))
          0))

  (define t3
    (flat '(2 2 3 4)
          (build-vector 48
            (λ (i)
              (* (quotient i 24) i)))
          0))

  (define t4
    (flat '(2 2 3 4)
          (build-vector 48
            (λ (i)
              (- (* 2.000001 (* (quotient i 24) i)) 48.0)))
          0))

  (check-true (equal-elements? t0 t1))

  (check-true (equal-elements? t0 t2)) ;; elements are equal, but shapes are not

  (check-true (equal-elements? t0 (flat '(2 3 4)
                                        (flat-store t2)
                                        0)))

  (check-false (equal-elements? t1 (flat '(2 3 4)
                                        (flat-store t3)
                                        24)))

  (check-true (equal-elements? t1 (flat '(2 3 4)
                                        (flat-store t4)
                                        24)))

  (check-true (tensor-equal? t0 t1))

  (check-false (tensor-equal? t0 t2)) ;; elements are equal, but shapes are not

  (check-true (tensor-equal? t0 (flat '(2 3 4)
                                        (flat-store t2)
                                        0)))

  (check-false (tensor-equal? t1 (flat '(2 3 4)
                                        (flat-store t3)
                                        24)))

  (check-true (tensor-equal? t1 (flat '(2 3 4)
                                        (flat-store t4)
                                        24)))

  (check-tensor-equal? t0 t1)

  (check-tensor-equal? t0 (flat '(2 3 4)
                                (flat-store t2)
                                0))

  (check-tensor-equal? t1 (flat '(2 3 4)
                                (flat-store t4)
                                24)))
