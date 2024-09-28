(module+ test
  (require rackunit)
  (let ((k0 end-of-chain))
    (let ((dual0 0)
          (dual1 (dual 1 k0)))

      (check-equal? dual1 (dual 1 k0))
      (check-true (dual? dual1))
      (check-false (dual? 1))
      (check-equal? (ρ dual1) 1)
      (check-equal? (ρ dual0) 0)
      (check-equal? (κ dual1) k0)

      (check-equal? (map* (λ (d) (ρ d)) (∇-once dual1 (list dual0 dual1)))
                    '(0.0 1.0)))))
