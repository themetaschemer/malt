(module+ test
  (require rackunit)

  (define r0-td 3.0)
  (define r1-td (tensor 3.0 4.0 5.0))
  (define r2-td (tensor (tensor 3.0 4.0 5.0) (tensor 7.0 8.0 9.0)))
  (define r3-td
    (tensor (tensor (tensor 0 1) (tensor 2 3) (tensor 4 5))
            (tensor (tensor 6 7) (tensor 8 9) (tensor 10 11))
            (tensor (tensor 12 13) (tensor 14 15) (tensor 16 17))
            (tensor (tensor 18 19) (tensor 20 21) (tensor 22 23))))
  (define test-shape (list 2 2 3))

  (check-equal? (shape r0-td) (list))
  (check-equal? (shape r1-td) (list 3))
  (check-equal? (shape r2-td) (list 2 3))

  (check-equal? (rank r0-td) 0)
  (check-equal? (rank r1-td) 1)
  (check-equal? (rank r2-td) 2)

  (check-equal? (size-of '()) 1)
  (check-equal? (size-of test-shape) 12)


  (check-equal? (size-of '(4 3 2)) 24)

  (check-equal? (deep-tref r3-td '(3 2 1)) 23)
  (check-equal? (deep-tref r3-td '(2 1 0)) 14)

  (check-equal? (invert-reference '(6 2 1) 23) '(3 2 1))
  (check-equal? (invert-reference '(6 2 1) 14) '(2 1 0))

  (check-equal? (strides '(4 3 2)) '(6 2 1))
  (check-equal? (strides '()) '())

  (check-equal? (flat-reference '(6 2 1) '(2 1 0) 0) 14)
  (check-equal? (flat-ref '(6 2 1) '(2 1 0)) 14)
  (check-equal? (convert-idx (strides '(24)) '(6 2 1) '(2 1 0))
                '(14))
  (check-equal? (reshape-tensor '(24) r3-td)
                (tensor  0  1  2  3  4  5
                         6  7  8  9 10 11
                         12 13 14 15 16 17
                         18 19 20 21 22 23))

  (check-equal? (reshape '(24) r3-td)
                (tensor  0  1  2  3  4  5
                         6  7  8  9 10 11
                         12 13 14 15 16 17
                         18 19 20 21 22 23))

  (check-equal? (reshape '(4 1) (tensor 0 1 2 3))
                (tensor (tensor 0) (tensor 1) (tensor 2) (tensor 3)))

  (check-equal? (reshape '(6) r2-td)
                (tensor 3.0 4.0 5.0 7.0 8.0 9.0))

  (check-equal? (reshape '(3 2) r2-td)
                (tensor (tensor 3.0 4.0)
                        (tensor 5.0 7.0)
                        (tensor 8.0 9.0)))

    (check-exn exn:fail?
             (λ ()
               (tensor "1 2" 1 2)))

  (check-exn exn:fail?
             (λ ()
               (tensor)))

  (check-exn exn:fail?
             (λ ()
               (tensor 1 (tensor 2 3))))

  (check-exn exn:fail?
             (λ ()
               (tensor tensor (tensor 2 3)))))
