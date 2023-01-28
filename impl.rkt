#lang racket
(begin-for-syntax
  (require racket/dict
           racket/port
           racket/pretty))



(define-for-syntax settings
  (make-parameter #f))

;;--------------------------------
;; Preferences reader
;;--------------------------------

(define-for-syntax read-preferences
  (λ (fname)
    (let ((port (make-parameter #f)))
      (dynamic-wind
        (λ () (and (file-exists? fname) (port (open-input-file fname))))
        (λ () (or (and (port) (port->list read (port)))
                  default-preferences))
        (λ () (when (port) (close-input-port (port))))))))

(define-for-syntax init-settings
  (λ ()
    (settings (make-hash (read-preferences "local.cfg")))))

;;--------------------------------
;; Config params so far
;;--------------------------------

(define-for-syntax tensor-implementation
  (λ ()
    (car (dict-ref (settings) 'tensor-implementation))))

;; Default settings
(define-for-syntax default-preferences
  `((tensor-implementation flat-tensors)))

(begin-for-syntax
  (when (not (settings))
    (init-settings)))

;;--------------------------------
;; Config params so far
;;--------------------------------
(define-syntax load-tensors
  (λ (x)
    (printf "Tensor implementation: ~s~%" (tensor-implementation))
    #`(begin
        #,(case (tensor-implementation)
            ((flat-tensors) #'(require "flat-tensors.rkt"))
            ((nested-tensors) #'(require "nested-tensors.rkt")))
        #,(case (tensor-implementation)
            ((flat-tensors) #'(provide (all-from-out "flat-tensors.rkt")))
            ((nested-tensors) #'(provide (all-from-out "nested-tensors.rkt")))))))

(load-tensors)
