(module+ test
  (require rackunit)
  (require (only-in "../tensors.rkt" tensor))
  (require (only-in "../autodiff.rkt" check-ρ-∇ check-dual-equal?))
  (require (only-in "A-scalar-ops.rkt" d*))

  (define r2-t1 (tensor (tensor 3.0 4.0) (tensor 5.0 6.0)))
  (define r1-t2 (tensor 5.0 6.0 7.0))
  (define r1-t1 (tensor 3.0 4.0 5.0 6.0 7.0))

  (check-dual-equal?
   (d-concat r2-t1 r1-t2)
   (tensor (tensor 3.0 4.0 5.0 6.0 7.0)
           (tensor 5.0 6.0 5.0 6.0 7.0)))

    (check-ρ-∇ ((λ (t1 t2 t3) (d* t3 (d-concat t1 t2))) r2-t1 r1-t2 r1-t1)
               (tensor (tensor 9.0 16.0 25.0 36.0 49.0)
                       (tensor 15.0 24.0 25.0 36.0 49.0))
               (list (tensor (tensor 3.0 4.0) (tensor 3.0 4.0))
                     (tensor 10.0 12.0 14.0)
                     (tensor 8.0 10.0 10.0 12.0 14.0))))
