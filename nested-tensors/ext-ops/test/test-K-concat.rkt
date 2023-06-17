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
                   (tensor 8.0 10.0 10.0 12.0 14.0)))
  (define r3-t1
    (tensor (tensor (tensor 1.0 2.0)
                    (tensor 3.0 4.0)
                    (tensor 5.0 6.0)
                    (tensor 7.0 8.0))

            (tensor (tensor 9.0 10.0)
                    (tensor 11.0 12.0)
                    (tensor 13.0 14.0)
                    (tensor 15.0 16.0))

            (tensor (tensor 17.0 18.0)
                    (tensor 19.0 20.0)
                    (tensor 21.0 22.0)
                    (tensor 23.0 24.0))))


  (define r2-t2
    (tensor (tensor 1.0 2.0)
            (tensor 3.0 4.0)
            (tensor 5.0 6.0)
            (tensor 7.0 8.0)))

  (define r1-t3
    (tensor 0.5 0.5))

  (define concat-2 (d-concat-n 2))

  (check-dual-equal?
   (concat-2 r3-t1 r2-t2)
   (tensor (tensor (tensor 1.0 2.0)
                   (tensor 3.0 4.0)
                   (tensor 5.0 6.0)
                   (tensor 7.0 8.0)
                   (tensor 1.0 2.0)
                   (tensor 3.0 4.0)
                   (tensor 5.0 6.0)
                   (tensor 7.0 8.0))

           (tensor (tensor 9.0 10.0)
                   (tensor 11.0 12.0)
                   (tensor 13.0 14.0)
                   (tensor 15.0 16.0)
                   (tensor 1.0 2.0)
                   (tensor 3.0 4.0)
                   (tensor 5.0 6.0)
                   (tensor 7.0 8.0))

           (tensor (tensor 17.0 18.0)
                   (tensor 19.0 20.0)
                   (tensor 21.0 22.0)
                   (tensor 23.0 24.0)
                   (tensor 1.0 2.0)
                   (tensor 3.0 4.0)
                   (tensor 5.0 6.0)
                   (tensor 7.0 8.0))))


  (check-ρ-∇ ((λ (t1 t2 t3) (d* t3 (concat-2 t1 t2))) r3-t1 r2-t2 r1-t3)
             (tensor (tensor (tensor 0.5 1.0)
                             (tensor 1.5 2.0)
                             (tensor 2.5 3.0)
                             (tensor 3.5 4.0)
                             (tensor 0.5 1.0)
                             (tensor 1.5 2.0)
                             (tensor 2.5 3.0)
                             (tensor 3.5 4.0))

                     (tensor (tensor 4.5 5.0)
                             (tensor 5.5 6.0)
                             (tensor 6.5 7.0)
                             (tensor 7.5 8.0)
                             (tensor 0.5 1.0)
                             (tensor 1.5 2.0)
                             (tensor 2.5 3.0)
                             (tensor 3.5 4.0))

                     (tensor (tensor 8.5 9.0)
                             (tensor 9.5 10.0)
                             (tensor 10.5 11.0)
                             (tensor 11.5 12.0)
                             (tensor 0.5 1.0)
                             (tensor 1.5 2.0)
                             (tensor 2.5 3.0)
                             (tensor 3.5 4.0)))
             (list
              (tensor (tensor (tensor 0.5 0.5)
                              (tensor 0.5 0.5)
                              (tensor 0.5 0.5)
                              (tensor 0.5 0.5))
                      (tensor (tensor 0.5 0.5)
                              (tensor 0.5 0.5)
                              (tensor 0.5 0.5)
                              (tensor 0.5 0.5))
                      (tensor (tensor 0.5 0.5)
                              (tensor 0.5 0.5)
                              (tensor 0.5 0.5)
                              (tensor 0.5 0.5)))

              (tensor (tensor 1.5 1.5)
                      (tensor 1.5 1.5)
                      (tensor 1.5 1.5)
                      (tensor 1.5 1.5))

              (tensor 192.0 216.0))))
