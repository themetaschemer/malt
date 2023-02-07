#lang racket

(require "base.rkt")

(define raw-tensor-printing? (make-parameter #f))
(define correct-ports
  (λ ()
    (let ((op (current-output-port))
          (ep (current-error-port)))
      (let ((port-correct (λ (f port) (f port (port-corrector (f port))))))
        (port-correct (λ (p . c) (apply global-port-print-handler c)) op)
        (port-correct (λ (p . c) (apply global-port-print-handler c)) ep)
        (port-correct port-write-handler op)
        (port-correct port-display-handler op)
        (port-correct port-print-handler op)
        (port-correct port-write-handler ep)
        (port-correct port-display-handler ep)
        (port-correct port-print-handler ep)))))

(define port-corrector
  (λ (cp)
    (λ (x port . opt)
      (cond
        ((raw-tensor-printing?) (apply cp x port opt))
        ((null? opt) (cp (make-printable x) port))
        (else (cp (make-printable x) port (car opt)))))))

(define new-pretty-print-size-hook
  (let ((ppsh (pretty-print-size-hook)))
    (λ (v display? port)
      (cond
        ((raw-tensor-printing?) (ppsh v display? port))
        ((vector? v) (string-length (~a (make-printable v))))
        (else (ppsh v display? port))))))

(define new-pretty-print-print-hook
  (let ((ppph (pretty-print-print-hook)))
    (λ (v display? port)
      (cond
        ((raw-tensor-printing?) (ppph v display? port))
        ((and display? (vector? v)) (display (make-printable v) port))
        ((and (not display?) (vector? v)) (write (make-printable v) port))
        (else (ppph v display? port))))))

(define pretty-print-handler
  (λ (v)
    (cond
      ((void? v) (void))
      ((raw-tensor-printing?) (pretty-print v))
      (else (pretty-print (make-printable v))))))

(pretty-print-print-hook new-pretty-print-print-hook)
(pretty-print-size-hook new-pretty-print-size-hook)
(current-print pretty-print-handler)

(correct-ports)

(provide raw-tensor-printing?)
