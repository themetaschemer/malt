(module+ test
  (require rackunit)

  (define test-samples
    (samples 10 3))

  (check-true (= (length test-samples) 3))
  (check-true (andmap (λ (sample) (>= sample 0) (<= sample 9)) test-samples))

  (define test-expectant-fn
    (λ (xs ys)
      (λ (theta)
        (let ((sx (shape xs))
              (sy (shape ys)))
          (check-equal? (car sx) (car sy))
          (check-dual-equal? (sum (=-1 xs ys)) 3.0)))))

  (define test-tensor (tensor 1.0 2.0 3.0 4.0 5.0
                              1.0 2.0 3.0 4.0 5.0))

  ;; Using the same test tensor allows us to ensure sampling-obj
  ;; behaves correctly regardless of which samples are picked
  (with-hyper ((batch-size 3))
    ((sampling-obj test-expectant-fn test-tensor test-tensor) #f)))
