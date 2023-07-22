#lang racket

(require setup/setup)

(define config-file-path
  (collection-file-path "local.cfg" "malt"))

(define set-impl
  (λ (impl)
    (when (not (member impl '(learner nested-tensors flat-tensors lazy)))
      (error "Unknown implementation: ~a~%" impl))
    (setup #:collections (list (list "malt")) #:clean? #t)
    (write-implementation-to-config-file impl)
    (setup #:collections (list (list "malt")))))

(define write-implementation-to-config-file
  (λ (impl)
    (let ((port #f))
      (dynamic-wind
        (λ ()
          (set! port (open-output-file config-file-path #:exists 'truncate)))
        (λ ()
          (fprintf port "(tensor-implementation ~a)~%" impl))
        (λ ()
          (close-output-port port))))))

(provide set-impl)
