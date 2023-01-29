#lang racket

(require "A-autodiff.rkt")

(define prim1
  (λ (ρ-fn ∇-fn)
    (λ (da)
      (let ((ra (ρ da)))
        (dual (ρ-fn ra)
          (λ (d z σ)
            (let ((ga (∇-fn ra z)))
              ((κ da) da ga σ))))))))

(define prim2
  (λ (ρ-fn ∇-fn)
    (λ (da db)
      (let ((ra (ρ da))
            (rb (ρ db)))
        (dual (ρ-fn ra rb)
          (λ (d z σ)
            (let-values (((ga gb) (∇-fn ra rb z)))
              (let ((σ-hat ((κ da) da ga σ)))
                ((κ db) db gb σ-hat)))))))))

(provide prim1 prim2)
