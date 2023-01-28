(module+ test
  (require rackunit)

  (define bf1
    (λ (t)
      (λ (theta)
        (* (ref theta 0) t))))

  (define bf2
    (λ (t)
      (λ (theta)
        (* (ref theta 0) t))))

  (define bf1bf2
    (compose-block-fns bf1 bf2 1))

  (check-dual-equal? ((bf1bf2 4.0) '(2.0 4.0)) 32.0)

  (define b1 (block bf1 '(())))
  (define b2 (block bf2 '(())))

  (define b1b2 (stack2 b1 b2))

  (check-dual-equal? (((block-fn b1b2) 4.0) '(2.0 4.0)) 32.0)

  (define b1b2-again (stack-blocks (list b1 b2)))

  (check-dual-equal? (((block-fn b1b2-again) 4.0) '(2.0 4.0)) 32.0))
