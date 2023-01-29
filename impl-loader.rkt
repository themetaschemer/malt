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
    (settings (make-hash (read-preferences "local.cfg")))))

;;--------------------------------
;; Config params so far
;;--------------------------------

(define tensor-implementation
  (λ ()
    (car (dict-ref (settings) 'tensor-implementation))))

;; Default settings
(define default-preferences
  `((tensor-implementation flat-tensors)))

(when (not (settings))
  (init-settings))

(provide tensor-implementation)
