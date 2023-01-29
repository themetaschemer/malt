(module+ test
  (require rackunit)
  (require "../autodiff.rkt")

  (check-dual-equal? (rectify-0 3.0) 3.0)
  (check-dual-equal? (rectify-0 -3.0) 0.0)

  (require "A-scalar-ops.rkt")
  (check-dual-equal? (rectify-0 (+-0-0 0 -3.0)) 0.0)
  (check-dual-equal? (rectify-0 (*-0-0 1 -3.0)) 0.0)

  (check-dual-equal? (rectify-0 (+-0-0 0 3.0)) 3.0)
  (check-dual-equal? (rectify-0 (*-0-0 1 3.0)) 3.0)

  (check-dual-equal? (rectify (tensor 1.0 2.3 -1.1))
                     (tensor 1.0 2.3 0.0))

  (check-dual-equal? (rectify (d+ (tensor 1.0 2.3 -1.1)
                                  (tensor 1.0 2.3 -1.1)))
                     (tensor 2.0 4.6 0.0)))
