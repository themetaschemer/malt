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

  (check-equal?
   (desc-u
    (λ (t e)
      (+! t e))
    (tensor 0 1 2 3)
    (tensor 4 5 6 7))
   (tensor (tensor 4 5 6 7)
           (tensor 5 6 7 8)
           (tensor 6 7 8 9)
           (tensor 7 8 9 10)))

  (check-equal?
   (desc-t
    (λ (e u)
      (+! e u))
    (tensor 4 5 6 7)
    (tensor 0 1 2 3))
   (tensor (tensor 4 5 6 7)
           (tensor 5 6 7 8)
           (tensor 6 7 8 9)
           (tensor 7 8 9 10)))

  (check-equal? (+! (tensor 1 2 3 4) 2)
                (tensor 3 4 5 6))

  (let-values (((gt gu) (fill-gu-acc-gt
                          (make-vector 3 0.0)
                          (λ (i) (values (+ i 1.0) (+ i 2.0)))
                          2
                          0.0)))
    (check-equal? gu (tensor 2.0 3.0 4.0))
    (check-equal? gt 6.0))

  (let-values (((gt gu) (build-gu-acc-gt 3
                          (λ (i) (values (+ i 1.0) (+ i 2.0))))))
    (check-equal? gu (tensor 2.0 3.0 4.0))
    (check-equal? gt 6.0))

  (let-values (((gt gu) (fill-gt-acc-gu
                          (make-vector 3 0.0)
                          (λ (i) (values (+ i 2.0) (+ i 1.0)))
                          2
                          0.0)))
    (check-equal? gt (tensor 2.0 3.0 4.0))
    (check-equal? gu 6.0))

  (let-values (((gt gu) (build-gt-acc-gu 3
                          (λ (i) (values (+ i 2.0) (+ i 1.0))))))
    (check-equal? gt (tensor 2.0 3.0 4.0))
    (check-equal? gu 6.0))

  (let-values (((gt gu) (fill-gt-gu
                          (make-vector 3 0.0)
                          (make-vector 3 0.0)
                          (λ (i) (values (+ i 2.0) (+ i 1.0)))
                          2)))
    (check-equal? gt (tensor 2.0 3.0 4.0))
    (check-equal? gu (tensor 1.0 2.0 3.0)))

  (let-values (((gt gu) (build-gt-gu 3
                          (λ (i) (values (+ i 2.0) (+ i 1.0))))))
    (check-equal? gt (tensor 2.0 3.0 4.0))
    (check-equal? gu (tensor 1.0 2.0 3.0)))

  (let-values (((gt gu) (desc-u-∇
                         (λ (t ui zi)
                           (values (* zi ui) (* zi t)))
                         6.0
                         (tensor 2.0 3.0 4.0)
                         (tensor 1.0 1.0 1.0))))
    (check-equal? gt 9.0)
    (check-equal? gu (tensor 6.0 6.0 6.0)))

  (let-values (((gt gu) (desc-t-∇
                         (λ (ti u zi)
                           (values (* zi u) (* zi ti)))
                         (tensor 2.0 3.0 4.0)
                         6.0
                         (tensor 1.0 1.0 1.0))))
    (check-equal? gt (tensor 6.0 6.0 6.0))
    (check-equal? gu 9.0))

  (let-values (((gt gu) (tmap2
                         (λ (ti ui zi)
                           (values (* zi ui) (* zi ti)))
                         (tensor 2.0 3.0 4.0)
                         (tensor 1.0 2.0 3.0)
                         (tensor 1.0 1.0 1.0))))
    (check-equal? gt (tensor 1.0 2.0 3.0))
    (check-equal? gu (tensor 2.0 3.0 4.0)))

  (define *∇ (ext2-∇ (λ (a b z) (values (* z b) (* z a)))
                     0
                     0))
  (let-values (((gt gu) (*∇ (tensor 2.0 3.0 4.0) (tensor 1.0 2.0 3.0) (tensor 1.0 1.0 1.0))))
    (check-equal? gt (tensor 1.0 2.0 3.0))
    (check-equal? gu (tensor 2.0 3.0 4.0)))

  (define sum-1-∇
    (λ (t z)
      (tmap (λ (ti) z) t)))

  (define sum-∇ (ext1-∇ sum-1-∇ 1))

  (let ((gt (sum-∇ (tensor 2.0 3.0 4.0)
                   1.0)))
    (check-equal? gt (tensor 1.0 1.0 1.0)))

  (let ((gt (sum-∇ (tensor (tensor 2.0 3.0 4.0)
                           (tensor 2.0 3.0 4.0))
                   (tensor 2.0 1.0))))
    (check-equal? gt (tensor (tensor 2.0 2.0 2.0)
                             (tensor 1.0 1.0 1.0))))
  )
