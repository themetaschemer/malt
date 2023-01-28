#lang racket

(define-syntax (declare-hyper x)
  (syntax-case x ()
    ((_ name)
     (with-syntax ((setter (get-setter-name #'name #'name))
                   (unset-value (datum->syntax x
                                  (string->symbol
                                    (format "unset-hyper-~a" (syntax->datum #'name))))))
       #'(begin
           (define name 'unset-value)
           (define (setter v)
             (set! name v))
           (provide name setter))))))

(define-syntax (with-hypers x)
  (syntax-case x ()
    ((_ ((h v) ...) body bodys ...)
     (with-syntax
         (((setter ...)
           (map (λ (x) (get-setter-name x x)) (syntax->list #'(h ...))))
          ((saver ...)
           (map (λ (x) (get-saver-name x x)) (syntax->list #'(h ...))))
          (show-hypers (datum->syntax x 'show-hypers)))
       #'(let ((saver h) ...)
           (dynamic-wind
             (λ () (setter v) ...)
             (λ () (let ((show-hypers
                          (lambda ()
                            (report-hypers (list 'h ...)
                                           (list h ...)))))
                     body bodys ...))
             (λ () (setter saver) ...)))))))

(define-for-syntax (get-setter-name ctx name)
  (datum->syntax ctx
    (string->symbol
      (format "set-~a!" (syntax->datum name)))))

(define-for-syntax (get-saver-name ctx name)
  (datum->syntax ctx
    (string->symbol
      (format "saved-~a" (syntax->datum name)))))

(define-syntax declare-hypers
  (syntax-rules ()
    ((declare-hypers h ...)
     (begin (declare-hyper h) ...))))

(define-syntax with-hyper
  (syntax-rules ()
    ((with-hyper ((h v)) body bodys ...)
     (with-hypers ((h v)) body bodys ...))))

(define-syntax grid-search
  (syntax-rules ()
    [(grid-search good-enough?
                 ((hyper val vals ...) ...)
                 body0 body ...)
     (let loop ((hs (cartesian-product
                      (list (list val vals ...) ...))))
       (cond
         ((null? hs) #f)
         (else
          (let ((theta (hyper-bindings
                        (hyper ...)
                        (car hs)
                        (begin body0 body ...))))
            (cond
              ((good-enough? theta)
               (report-hypers '(hyper ...) (car hs))
               theta)
              (else (loop (cdr hs))))))))]))

(define-syntax hyper-bindings
  (syntax-rules ()
    [(_ (hyper ...) h body)
     (hyper-bindings (hyper ...) h body 0 ())]
    [(_ () h body i bindings)
     (with-hypers bindings body)]
    [(_ (hyper hypers ...) h body i (bs ...))
     (hyper-bindings (hypers ...) h body (add1 i)
       (bs ... (hyper (list-ref h i))))]))

(define cartesian-product
  (λ (lh)
    (cond
      ((null? (cdr lh))
       (map list (car lh)))
      (else
       (one-product (car lh) (cartesian-product (cdr lh)))))))

(define one-product
  (λ (column prod)
    (cond
      ((null? column) (list))
      (else
       (append
         (map (λ (p) (cons (car column) p)) prod)
         (one-product (cdr column) prod))))))

(define (report-hypers hypers values)
  (fprintf (current-error-port)
    "*************** HYPERS ***************~%")
  (for ([h hypers]
        [v values])
    (fprintf (current-error-port) "~a: ~a~%" h v))
  (fprintf (current-error-port)
    "**************************************~%"))

(include "test/test-A-hypers.rkt")

(provide with-hyper with-hypers
         declare-hyper declare-hypers grid-search)
