(module+ test
  (require rackunit)

  (define r0-td 3.0)
  (define r1-td (tensor 3.0 4.0 5.0))
  (define r2-td (tensor (tensor 3.0 4.0 5.0) (tensor 7.0 8.0 9.0)))
  (define test-shape (list 2 2 3))

  (check-equal? (tmap (λ (x) (+ x 1)) r1-td) (tensor 4.0 5.0 6.0))

  (check-true (rank> r2-td r1-td))
  (check-false (rank> r1-td r2-td))

  (check-true (of-ranks? 1 r1-td 2 r2-td))
  (check-false (of-ranks? 2 r1-td 2 r2-td))

  (check-true (of-rank? 2 r2-td))
  (check-false (of-rank? 1 r2-td))

  (define +-ρ (ext2 + 0 0))

  (check-equal?
   (desc-u
    (λ (t e)
      (+-ρ t e))
    (tensor 0 1 2 3)
    (tensor 4 5 6 7))
   (tensor (tensor 4 5 6 7)
           (tensor 5 6 7 8)
           (tensor 6 7 8 9)
           (tensor 7 8 9 10)))

  (check-equal?
   (desc-t
    (λ (e u)
      (+-ρ e u))
    (tensor 4 5 6 7)
    (tensor 0 1 2 3))
   (tensor (tensor 4 5 6 7)
           (tensor 5 6 7 8)
           (tensor 6 7 8 9)
           (tensor 7 8 9 10)))

  (check-equal? (+-ρ (tensor 1 2 3 4) 2)
                (tensor 3 4 5 6)))
