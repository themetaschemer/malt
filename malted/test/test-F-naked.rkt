(module+ test
  (require rackunit)
  (require "../base.rkt")

  (define obj (λ (theta)
                (sqr (- 30 (ref theta 0)))))

  (check-within
   (with-hypers ((revs 400)
                 (alpha 0.01))
     (naked-gradient-descent obj (list 3.0)))
   '(29.991647931623252)
   (tolerance)))
