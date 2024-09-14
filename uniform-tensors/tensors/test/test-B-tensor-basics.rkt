(module+ test
  (require rackunit)
  (require "A-equality.ss")

  (define r0-td 3.0)
  (define r1-td (tensor 3.0 4.0 5.0))
  (define r2-td (tensor (tensor 3.0 4.0 5.0) (tensor 7.0 8.0 9.0)))
  (define r3-td
    (tensor (tensor (tensor 0 1) (tensor 2 3) (tensor 4 5))
            (tensor (tensor 6 7) (tensor 8 9) (tensor 10 11))
            (tensor (tensor 12 13) (tensor 14 15) (tensor 16 17))
            (tensor (tensor 18 19) (tensor 20 21) (tensor 22 23))))

  (check-tensor-equal? (tref r1-td 2) 5.0)
  (check-equal? (tlen r1-td) 3)
  (check-tensor-equal? (list->tensor (list 3.0 4.0 5.0)) r1-td)

  (check-true (and (tensor? r0-td) (tensor? r1-td)))
  (check-false (tensor? '(a b c)))

  (check-tensor-equal? (build-tensor '(4 3 2)
                                     (λ (idx)
                                       (+ (* 6 (ref idx 0))
                                          (* 2 (ref idx 1))
                                          (ref idx 2))))
                       r3-td)

  (check-tensor-equal? (build-tensor '(1 2 3) (λ (idx) (+ (list-ref idx 0) (list-ref idx 1) (list-ref idx 2))))
                       (tensor (tensor (tensor 0 1 2) (tensor 1 2 3))))

  (check-tensor-equal? (trefs r1-td '(0 2)) (tensor 3.0 5.0))

  )
