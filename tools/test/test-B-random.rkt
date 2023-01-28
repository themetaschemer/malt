(module+ test
  (require rackunit)

  ;; Safeguard tests to prevent inadvertant errors
  (check-equal? (phi 1) 0.24197072451914337)
  (check-equal? (phi 2.33) 0.026426485497261724)
  (check-equal? (phi -2.33) (phi 2.33))

  ;; Since most functions in this file all use (random)
  ;; it is hard to define an exact test, so we use a proxy
  ;; We generate a large number of random numbers using
  ;; a normal distribution with a given mean and variance
  ;; and then we test that the generated numbers have the
  ;; said mean and variance within some tolerance

  (define count 100000)
  (define (mean-std n [mu 0.0][sigma 1.0])
    (let-values (((sum ssr)
                  (for/fold ([sum 0][sum-squares 0]) ((c (in-range n)))
                    (let ((r (random-normal mu sigma)))
                      (values (+ sum r) (+ sum-squares (sqr r)))))))
      (let ((mean (/ sum n)))
        (values mean
                (sqrt (- (/ ssr n) (sqr mean)))))))

  ;; We keep tolerance little higher here so we
  ;; can have a smaller count for reasonable test times
  ;;
  ;; Tolerance for std-deviation is a multiple
  ;; of the target std-deviation.

  (let-values (((mean std) (mean-std count)))
    (check-true (and (>= mean -0.05) (<= mean 0.05)))
    (check-true (and (>= std 0.95) (<= std 1.05))))

  (let-values (((mean std) (mean-std count 3 7)))
    (check-true (and (>= mean 2.9) (<= mean 3.1)))
    (check-true (and (>= std 6.7) (<= std 7.3))))

  (let-values (((mean std) (mean-std count 3.2 4.7)))
    (check-true (and (>= mean 3.1) (<= mean 3.3)))
    (check-true (and (>= std 4.5) (<= std 4.9)))))
