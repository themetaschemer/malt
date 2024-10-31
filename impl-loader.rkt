#lang racket

(require racket/dict
         racket/port
         racket/pretty)

(define settings
  (make-parameter #f))

;;--------------------------------
;; Preferences reader
;;--------------------------------

(define read-preferences
  (λ (fname)
    (let ((port (make-parameter #f)))
      (dynamic-wind
        (λ () (and (file-exists? fname) (port (open-input-file fname))))
        (λ () (or (and (port) (port->list read (port)))
                  default-preferences))
        (λ () (when (port) (close-input-port (port))))))))

(define init-settings
  (λ ()
    (settings (make-hash (read-preferences (or (getenv "MALT_PREFERENCES") "local.cfg"))))))

;;--------------------------------
;; Config params so far
;;--------------------------------

(define get-param
  (λ (param)
    (match (car (dict-ref (settings) param))
      [`(getenv ,name) (getenv name)]
      [x x])))

(define (get-boolean-param bparam)
  (match (get-param bparam)
      ("yes" #t)
      (#t #t)
      (_ #f)))

(define tensor-implementation
  (λ ()
    (get-param 'tensor-implementation)))

(define accelerate?
  (λ ()
    (get-param 'accelerate?)))

(define debug-kernel?
  (λ ()
    (get-boolean-param 'debug-kernel?)))

(define opencl-device-type
  (λ ()
    (match (get-param 'opencl-device-type)
      ["" 'CL_DEVICE_TYPE_DEFAULT]
      [(? string? s) (string->symbol s)]
      [(? symbol? x) x]
      [e (error 'opencl-device-type:err "Invalid paramater: ~a" e)])))

(define disable-unsafe-tests?
  (λ ()
    (get-boolean-param 'disable-unsafe-tests?)))

;; Default settings
(define default-preferences
  `((tensor-implementation learner)
    (accelerate? #t)
    (debug-kernel? (getenv "MALT_DEBUG_KERNEL"))
    (opencl-device-type (getenv "CL_DEVICE_TYPE"))
    (disable-unsafe-tests? (getenv "MALT_DISABLE_UNSAFE_TESTS"))))

(when (not (settings))
  (init-settings))

(provide tensor-implementation accelerate? debug-kernel? opencl-device-type disable-unsafe-tests?)
