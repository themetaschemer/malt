(module+ test
  (require rackunit)

  (check-equal? (revise add1 10 0) 10)

  (define obj (λ (theta)
                (sqr (- 30 (list-ref theta 0)))))

  (define id (λ (x) x))

  ;; update functions must use extended, non-dual operators
  (define naked-gd (gradient-descent id id
                                     (λ (theta g)
                                       (--ρ theta (*-ρ g alpha)))))
  (check-equal?
   (with-hypers ((revs 500)
                 (alpha 0.01))
     (naked-gd obj (list 3.0)))
   '(29.998892352401082)))
