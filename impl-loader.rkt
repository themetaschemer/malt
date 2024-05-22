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

(define tensor-implementation
  (λ ()
    (car (dict-ref (settings) 'tensor-implementation))))

(define accelerate?
  (λ ()
    (car (dict-ref (settings) 'accelerate?))))

(define debug-kernel?
  (λ ()
    (car (dict-ref (settings) 'debug-kernel?))))

;; Default settings
(define default-preferences
  `((tensor-implementation learner)
    (accelerate? #t)
    (debug-kernel? #f)))

(when (not (settings))
  (init-settings))

(provide tensor-implementation accelerate? debug-kernel?)
