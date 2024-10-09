(module+ test
  (require rackunit)
  (require (only-in "../tensors.rkt" tensor))

  ;; Check basic numericals
  (let ((a 2)
        (b 3))
    (check-ρ-∇ (d+ a b) 5 (list 1.0 1.0))
    (check-ρ-∇ (d- a b) -1 (list 1.0 -1.0))
    (check-ρ-∇ (d* a b) 6 (list 3.0 2.0))
    (check-ρ-∇ (d/ a b)
               2/3
               '(0.3333333333333333 -0.2222222222222222))
    (check-ρ-∇ (d-exp a) (exp 2) (list (exp 2)))
    (check-ρ-∇ (d-log a) (log 2) (list 0.5))
    (check-ρ-∇ (d-expt a b) 8
               (list 12.0 5.545177444479562))
    (check-ρ-∇ (d-sqrt a)
               (sqrt 2)
               (list 0.3535533905932738))

    (let* ((first-derivative ((∇¹ d-sqr) b)))
      (check-dual-equal? first-derivative (list 6.0)))

    (define z
      (lambda (x y)
        (d+ (d-log x) (d* x y))))

    (let ((c (dual* 2))
          (d (dual* 2)))
      (check-dual-equal?
       ((∇¹ z) c d)
       (list 2.5 2.0)))

    (check-dual-equal? ((∇¹ z) a a) (list 2.5 2.0)))

  ;; Check numericals with vector-duals

  (let ((a (tensor 2.0 3.0 4.0))
        (b (tensor 3.0 8.0 9.0)))
    (check-ρ-∇ (d+ a b) (tensor 5.0 11.0 13.0) (list (tensor 1.0 1.0 1.0) (tensor 1.0 1.0 1.0)))
    (check-ρ-∇ (d- a b) (tensor -1 -5 -5) (list (tensor 1.0 1.0 1.0) (tensor -1.0 -1.0 -1.0)))
    (check-ρ-∇ (d* a b) (tensor 6.0 24.0 36.0) (list (tensor 3.0 8.0 9.0) (tensor 2.0 3.0 4.0)))
    (check-ρ-∇ (d/ a b)
               (tensor (/ 2.0 3.0) (/ 3.0 8.0) (/ 4.0 9.0))
               (list (tensor 0.3333333333333333 0.125 0.1111111111111111)
                     (tensor -0.2222222222222222 -0.046875 -0.04938271604938271)))
    (check-ρ-∇ (d-exp a) (tensor (exp 2.0) (exp 3.0) (exp 4.0))
               (list (tensor (exp 2.0) (exp 3.0) (exp 4.0))))
    (check-ρ-∇ (d-log a) (tensor (log 2.0) (log 3.0) (log 4.0))
               (list (tensor 0.5 (/ 1 3.0) (/ 1 4.0))))
    (check-ρ-∇ (d-expt a b)
               (tensor (expt 2.0 3.0) (expt 3.0 8.0) (expt 4.0 9.0))
               (list (tensor 12.0 17496.0 589824.0)
                     (tensor 5.545177444479562 7207.9952259514685 363408.7490014126)))

    (check-ρ-∇ (d-sqrt a)
               (tensor (sqrt 2.0) (sqrt 3.0) (sqrt 4.0))
               (list (tensor 0.3535533905932738 0.28867513459481287 0.25)))

    (check-ρ-∇ (d-sqr b) (tensor 9.0 64.0 81.0) (list (tensor 6.0 16.0 18.0)))

    (define z
      (lambda (x y)
        (d+ (d-log x) (d* x y))))

    (let ((c (dual* (tensor 2.0 3.0 4.0)))
          (d (dual* (tensor 2.0 3.0 4.0))))
      (check-dual-equal?
       ((∇¹ z) c d)
       (list (tensor 2.5 3.3333333333333335 4.25) (tensor 2.0 3.0 4.0))))

    (check-dual-equal? ((∇¹ z) a a) (list (tensor 2.5 3.3333333333333335 4.25) (tensor 2.0 3.0 4.0))))

  ;; Check numericals with lists
  (let ((x (dual* 3))
        (y (dual* 2)))
    (let ((f d*))
      (check-ρ-∇ (d* x y) 6 '(2.0 3.0))
      (check-dual-equal?
       ((∇¹ (λ (m n)
              (list (f m m) (f n n))))
        x y)
       '(6.0 4.0))
      (check-dual-equal?
       ((∇¹ (λ (m n)
              (list
               (list (f m m) (f n n))
               (list (f m m) (f n n)))))
        x y)
       '(12.0 8.0))
      (check-dual-equal?
       ((∇¹ (λ (m n)
              (map (λ (m) (f m m))
                   (list m n))))
        x y)
       '(6.0 4.0))

      (check-dual-equal?
       ((∇¹ (λ (m n)
              (map f (list m n) (list m n))))
        x y)
       '(6.0 4.0))

      (check-dual-equal?
       ((∇¹ (λ (m n)
              (map f (list m n) (list n m))))
        x y)
       '(4.0 6.0))))

  (let ((a 7)
        (b (tensor 13)))
    (check-ρ-∇ (d+ a b) (tensor 20) (list 1.0 (tensor 1.0)))
    (check-ρ-∇ (d* a b) (tensor 91) (list 13.0 (tensor 7.0)))
    (check-ρ-∇ (d/ a b) (tensor 7/13) (list 0.07692 (tensor -0.04142))))

  (let ((a 7)
        (b (tensor 13 15)))
    (check-ρ-∇ (d+ a b) (tensor 20 22) (list 2.0 (tensor 1.0 1.0)))
    (check-ρ-∇ (d* a b) (tensor 91 105) (list 28.0 (tensor 7.0 7.0)))
    (check-ρ-∇ (d/ a b) (tensor 7/13 7/15)
               (list 0.14358 (tensor -0.04142 -0.03111)))))
