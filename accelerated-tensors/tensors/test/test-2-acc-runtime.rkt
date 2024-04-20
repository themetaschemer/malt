(module+ test
  (require rackunit)

  (for ((_ (in-range 100)))
    (with-opencl
        (λ ()
          (check-true (not (not (context))))
          (check-true (not (not (command-queue)))))))
  )
