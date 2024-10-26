(module+ test
  (require rackunit)
  (require (only-in "../base.rkt" ρ))

  (define v (init-shape (list 1000 4)))
  (define mean-v
    (abs (/ (sum (sum v)) 4000)))
  (define variance-v
    (- (/ (sum (sum (* v v))) 4000) (* mean-v mean-v)))
  (check-true (< (ρ mean-v) 0.05))
  (check-true (let ((forced (ρ variance-v)))
                (and (>= forced 0.4)
                     (<= forced 0.6))))

  ;; Here variance will be 2/8 = 0.25
  (define r (init-shape (list 1000 4 2)))
  (define mean-r (abs (/ (sum (sum (sum r))) 8000)))
  (define variance-r (- (/ (sum (sum (sum (* r r)))) 8000)
                        (* mean-r mean-r)))

  (check-true (< (ρ mean-r) 0.05))
  (check-true (let ((forced (ρ variance-r)))
                (and (>= forced 0.22)
                     (<= forced 0.28)))))
