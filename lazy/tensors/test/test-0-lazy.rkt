(module+ test
  (require rackunit)

  (define test-nested-tensor (tensor (tensor 1 2 3) (tensor 4 5 6) (tensor 7 8 9)))
  (check-true (bounded-idx*? test-nested-tensor (list 0 1)))
  (check-false (bounded-idx*? test-nested-tensor (list 1 3)))
  (check-false (bounded-idx*? test-nested-tensor (list 1 1 0))))
