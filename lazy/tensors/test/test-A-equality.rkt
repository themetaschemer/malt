(module+ test
  (require rackunit)

  (define t0
    (reshape '(2 3 4)
             (build-tensor
              '(24)
              (λ (i)
                (* 2.0 (car i))))))


  (define t1
    (reshape '(2 3 4)
             (build-tensor
              '(24)
              (λ (i)
                (* 2.000001 (car i))))))

  (define t2
    (reshape '(1 2 3 4)
             (build-tensor
              '(24)
              (λ (i)
                (* 2.000001 (car i))))))

  (define t3
    (reshape '(2 2 3 4)
             (build-tensor
              '(48)
              (λ (i)
                (* (quotient (car i) 24) (car i))))))

  (define t4
    (reshape '(2 2 3 4)
             (build-tensor
              '(48)
              (λ (i)
                (- (* 2.000001 (* (quotient (car i) 24) (car i))) 48.0)))))

  (check-true (tp-tensor-equal? t0 t1))

  (check-false (tp-tensor-equal? t0 t2)) ;; elements are equal, but shapes are not

  (check-true (tp-tensor-equal? t0 (reshape '(2 3 4)
                                         t2)))

  (tp-check-tensor-equal? t0 t1)

  (tp-check-tensor-equal? t0 (reshape '(2 3 4) t2)))
