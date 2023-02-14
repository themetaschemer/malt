#lang racket

(require malt)
(require "iris/data/iris-data.rkt")

(provide iris-train-xs iris-train-ys
         iris-test-xs iris-test-ys
         tll-iris-initial-theta
         run-tll-iris
         run-iris)

;;*----------------------------------------
;;* Network definitions
;;*----------------------------------------
(define dense-block
  (λ (n m)
    (block relu
      (list
        (list m n)
        (list m)))))

(define iris-network
  (stack-blocks
    (list
      (dense-block 4 8)
      (dense-block 8 3))))

(define iris-theta-shapes
  (block-ls iris-network))

(define iris-classifier
  (block-fn iris-network))

;;*----------------------------------------
;;* Some warnings
;;*----------------------------------------

(define print-note
  (λ ()
    (let ((cep (current-output-port)))
      (fprintf cep  "--------------------------------------------------------~%")
      (fprintf cep "A WORD OF ADVICE ABOUT IRIS~%")
      (fprintf cep "--------------------------------------------------------~%~%")
      (fprintf cep "The smallness of iris-network combined with the stochastic nature of init-theta~%")
      (fprintf cep "often causes variations in the final hyperparameters found by grid-search.~%")
      (fprintf cep "This means you might get results that are different from what are shown in the book.~%~%")
      (fprintf cep "As long as you get a high enough accuracy on the test set, any trained theta is acceptable.~%~%")
      (fprintf cep "It could also happen that once in a while, no working hyperparameter combination is found.~%")
      (fprintf cep "In that case please run the grid-search again.~%~%")
      (fprintf cep "Please refer to the Chapter Guide on https://www.thelittlelearner.com for further guidance.~%~%"))))

;;*----------------------------------------
;;* Grid searching
;;*----------------------------------------

(define accurate-enough-iris-theta?
  (λ (theta)
    (>= (accuracy (model iris-classifier theta) iris-test-xs iris-test-ys) 0.9)))

(define grid-search-iris-theta
  (λ ()
    (grid-search
     accurate-enough-iris-theta?
     ((revs 500 1000 2000 4000)
      (alpha 0.0001 0.0002 0.0005)
      (batch-size 4 8 16))
     (naked-gradient-descent
      (sampling-obj (l2-loss iris-classifier)
        iris-train-xs iris-train-ys)
      (init-theta iris-theta-shapes)))))

;;*----------------------------------------
;;* Kicking it all off
;;*----------------------------------------

(define run-iris
  (λ ()
    (let ((trained-theta (grid-search-iris-theta)))
      (cond
        (trained-theta
          (printf "Test accuracy: ~a~%"
            (accuracy (model iris-classifier trained-theta) iris-test-xs iris-test-ys))
         (printf "Trained theta:~%")
         (parameterize ((max-tensor-print-length 0)) ;; This makes sure the tensor is not truncated
           (pretty-print trained-theta)))
        (else
         ;; Sometimes the grid search can fail. If it does, try again.
         (printf "Grid search unsuccessful. Please try (run-iris) again.~%"))))))

;;*----------------------------------------
;;* TLL Book example run.
;;*----------------------------------------

(define tll-iris-initial-theta
  (list
   (tensor
    (tensor 0.4567374693020529 0.19828623224159106 -0.1791656741530271 -0.3010909419105787)
    (tensor -0.6085978529055036 -0.37813256632159414 0.6525919461799214 -0.02736258427588277)
    (tensor -0.15910077091878255 0.30935100240945007 -0.43223348220649294 0.44424201464211593)
    (tensor 0.29780171646282 0.27115067507001933 0.3512802108530173 -0.941133353767241)
    (tensor -0.6435366194048697 -0.7870457121505098 0.4672028162559846 -0.4060316748060222)
    (tensor 0.3542366127804169 -0.6294805381631496 1.2119983516222874 -0.48964923866459675)
    (tensor 0.29072501026246134 -0.11992778583131615 0.2716865689059567 0.5051197463327993)
    (tensor -0.05677192201680251 -0.8933344786252218 0.10639004770659627 -0.7276129460870265))
  (tensor 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
  (tensor
   (tensor 0.8360463658942785 0.21163937440648464 -0.36559830767572854 0.34006155051045595
           0.3095265146359776 -0.1585941540367561 0.33268716624682165 -0.5114119488395097)
   (tensor 0.15466255181586858 -0.26658077790718954 0.04571706722376748 0.10422918798466209
           -0.17593682447129064 0.6075530713389936 0.007216798991190192 -0.4698148147112468)
   (tensor 0.06636510408180833 -0.11501406598247131 0.7855953481117244 0.00849992094421447
           0.10415852852056427 0.4557511137599346 -0.029003952783791656 1.1873084795704665))
  (tensor 0.0 0.0 0.0)))

(define run-tll-iris
  (λ ()
    (let ((tll-iris-theta
           (with-hypers ((revs 2000)
                         (alpha 0.0002)
                         (batch-size 8))
             (naked-gradient-descent
              (sampling-obj (l2-loss iris-classifier)
                            iris-train-xs iris-train-ys)
              tll-iris-initial-theta))))
      (printf "TLL Test accuracy: ~a~%"
              (accuracy (model iris-classifier tll-iris-theta) iris-test-xs iris-test-ys))
      (printf "Trained theta:~%")
      (parameterize ((max-tensor-print-length 0))
        (pretty-print tll-iris-theta)))))

(print-note)
