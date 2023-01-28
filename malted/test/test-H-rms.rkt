(module+ test
  (require rackunit)
  (require "../base.rkt")

  (define obj (λ (theta)
                (sqr (- 30 (ref theta 0)))))

  (check-dual-equal?
   (with-hypers ((revs 170)
                 (alpha 0.1)
                 (beta 0.999))
     (rms-gradient-descent obj (list 3.0)))
   '(29.990436450964964))
  )
