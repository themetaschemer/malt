(module+ test
  (require rackunit)
  ;; TODO: Make this better. We musn't break abstraction boundaries
  (require "../lazy/tensors/0-lazy.rkt")

  (define v (init-shape (list 10 4)))
  (define mean-v
    (abs (/ (sum (sum v)) 40)))
  (define variance-v
    (- (/ (sum (sum (* v v))) 40) (* mean-v mean-v)))
  (check-true (< (force/eval mean-v) 0.05))
  (pretty-print (get-compiled variance-v))
  (check-true (let ((forced (force/eval variance-v)))
                (and (>= forced 0.4)
                     (<= forced 0.6))))

  ;; Here variance will be 2/8 = 0.25
  (define r (init-shape (list 10 4 2)))
  (define mean-r (abs (/ (sum (sum (sum r))) 80)))
  (define variance-r (- (/ (sum (sum (sum (* r r)))) 80)
                        (* mean-r mean-r)))

  (check-true (< (force/eval mean-r) 0.05))
  (pretty-print (get-compiled variance-r))
  (check-true (let ((forced (force/eval variance-r)))
                (println forced)
                (and (>= forced 0.22)
                     (<= forced 0.28)))))
