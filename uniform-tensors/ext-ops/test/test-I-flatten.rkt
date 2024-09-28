(module+ test
  (require rackunit)
  (require (only-in "../tensors.rkt" tensor))
  (require (only-in "../autodiff.rkt" check-ρ-∇ check-dual-equal?))
  (require (only-in "A-scalar-ops.rkt" d*))

  (define r2-t1 (tensor (tensor 3.0 4.0) (tensor 5.0 6.0)))
  (define r1-t1 (tensor 3.0 4.0 5.0 6.0))

  (check-dual-equal? (flatten-2 r2-t1) r1-t1)
  (check-ρ-∇ ((λ (t1 t2) (d* t1 (flatten-2 t2))) r1-t1 r2-t1)
             (tensor 9.0 16.0 25.0 36.0)
             (list (tensor 3.0 4.0 5.0 6.0) (tensor (tensor 3.0 4.0) (tensor 5.0 6.0)))))
