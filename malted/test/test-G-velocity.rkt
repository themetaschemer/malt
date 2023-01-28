(module+ test
  (require rackunit)
  (require "../base.rkt")

  (define obj (λ (theta)
                (sqr (- 30 (ref theta 0)))))

  (check-dual-equal?
   (with-hypers ((revs 70)
                 (alpha 0.01)
                 (mu 0.9))
     (velocity-gradient-descent obj (list 3.0)))
   '(30.686162582787535)))
