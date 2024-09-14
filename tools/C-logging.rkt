#lang racket

(require (only-in "../impl.rkt" ρ sum-ρ scalar? shape rank tensor +-ρ map*))

;; This is a logging system for tracking loss during training in Malt. It is Racket specific.
;; Logging in Racket is a pub/sub model. Publishers write their log events to a topic
;; and subscribers receive those (in a parallel thread, so that logging does not block)

(define-logger malt)

;; We use structured logs for all malt logging.
(define record
  (λ (name qty)
    (log-message malt-logger 'info
                 (logger-name malt-logger)
                 "recording"
                 (list name (ρ qty)))
    qty))

(define log-malt-reset
  (λ ()
    (log-message malt-logger 'info
                 (logger-name malt-logger)
                 "resetting"
                 '(reset))))

(define start-logging
  (λ ([sampling-frequency 20])
    (let ((log-receiver (make-log-receiver malt-logger 'debug 'malt)))
      (thread
       (λ ()
         (logging-looper log-receiver sampling-frequency))))))

;; Waits for a log message to arrive  on log-receiver
;; and handles it. Updates averages as necessary and
;; prints with the sampling frequency (which defaults to 20)
;; i.e., once every 20 pieces of data.

(define logging-looper
  (λ (log-receiver sampling-frequency)
    (let loop ([d0 0.0]
               [d1 0.0]
               [d2 0.0]
               [d3 0.0]
               [d4 0.0]
               [count 0]
               [sampling-count sampling-frequency])
      (let ((data (malt-log-handler (sync log-receiver))))
        (cond
          ((eq? data 'reset)
           (loop 0.0 0.0 0.0 0.0 0.0 0 sampling-frequency))
          ((and data (= sampling-count 1))
           (print-average (/ (ρ (sum-all d0 d1 d2 d3 d4 data))
                             (* 6 (ρ (product (shape data)))))
                          count)
           (loop d1 d2 d3 d4 data (add1 count) sampling-frequency))
          (data
           (loop d1 d2 d3 d4 data (add1 count) (sub1 sampling-count)))
          (else (loop d0 d1 d2 d3 d4 count sampling-count)))))))

(define product
  (λ (s)
    (apply * s)))

(define print-average
  (λ (avg count)
    (collect-garbage)
    (fprintf (current-error-port) "(~a ~a) [Memory: ~a][Window size 6]~%"
      (add1 count) avg
      (current-memory-use))))

(define sum-all
  (λ (d0 d1 d2 d3 d4 data)
    (sum-deep
      (+-ρ (map* ρ d0)
        (+-ρ (map* ρ d1)
          (+-ρ (map* ρ d2)
            (+-ρ (map* ρ d3)
              (+-ρ (map* ρ d4) (map* ρ data)))))))))

(define sum-deep
  (λ (t)
    (cond
      ((scalar? t) t)
      ((= 1 (rank t)) (sum-ρ t))
      (else (sum-deep (sum-ρ t))))))

;; Handles each log message appropriately.
(define malt-log-handler
  (λ (log-event)
    (let ([level (vector-ref log-event 0)]
          [message (vector-ref log-event 1)]
          [data (vector-ref log-event 2)]
          [topic (vector-ref log-event 3)])
      (match data
        (`(loss ,data) data)
        (`(reset) 'reset)
        (x
         (fprintf (current-error-port)
                  "[~a] ~a: ~a~%~s~%" topic level message x)
         #f)))))

(include "test/test-C-logging.rkt")

(provide record start-logging log-malt-reset
         log-malt-fatal
         log-malt-error
         log-malt-warning
         log-malt-info
         log-malt-debug)
