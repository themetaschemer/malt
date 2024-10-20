(module+ test
  (require rackunit)

  (for ((_ (in-range 100)))
    (λ ()
      (check-true (not (not (context))))
      (check-true (not (not (command-queue))))))
  )
