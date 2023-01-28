(module+ test
  (require rackunit)
  (require "../base.rkt")

  (define obj (λ (theta)
                (sqr (- 30 (ref theta 0)))))

  (check-dual-equal?
   (with-hypers ((revs 150)
                 (alpha 0.1)
                 (beta 0.999)
                 (mu 0.9))
     (adam-gradient-descent obj (list 3.0)))
   '(29.994907156105718))
  )
