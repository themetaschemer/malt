#lang racket
(require (only-in malt random-standard-normal list->tensor tlen tref rank scalar?))

(define dit-symbol '(1.0))
(define dah-symbol '(1.0 1.0 1.0))
(define inter-symbol-space '(0.0))

(define letter-length 16)

(define dit
  (λ rest
    (append dit-symbol
     (cond
       ((null? rest) '())
       (else (append inter-symbol-space
                     (apply (car rest) (cdr rest))))))))

(define dah
  (λ rest
    (append
     '(1.0 1.0 1.0)
     (cond
       ((null? rest) '())
       (else (append inter-symbol-space
                     (apply (car rest) (cdr rest))))))))

(define morse-code
  `((#\a . ,(dit dah))
    (#\b . ,(dah dit dit dit))
    (#\c . ,(dah dit dah dit))
    (#\d . ,(dah dit dit))
    (#\e . ,(dit))
    (#\f . ,(dit dit dah dit))
    (#\g . ,(dah dah dit))
    (#\h . ,(dit dit dit dit))
    (#\i . ,(dit dit))
    (#\j . ,(dit dah dah dah))
    (#\k . ,(dah dit dah))
    (#\l . ,(dit dah dit dit))
    (#\m . ,(dah dah))
    (#\n . ,(dah dit))
    (#\o . ,(dah dah dah))
    (#\p . ,(dit dah dah dit))
    (#\q . ,(dah dah dit dah))
    (#\r . ,(dit dah dit))
    (#\s . ,(dit dit dit))
    (#\t . ,(dah))
    (#\u . ,(dit dit dah))
    (#\v . ,(dit dit dit dah))
    (#\w . ,(dit dah dah))
    (#\x . ,(dah dit dit dah))
    (#\y . ,(dah dit dah dah))
    (#\z . ,(dah dah dit dit))))

(define embed-to
  (λ (len seq)
    (let ((n (- len (length seq))))
      (let* ((front (random n))
             (back (- n front)))
        (append (make-list front 0.0)
                seq
                (make-list back 0.0))))))

(define dB
  (λ (x)
    ;; x dB = 20 log (R)
    ;; R = 10^(x/20)
    (expt 10 (/ x 20))))

(define add-noise-and-remove-dc
  (λ (seq SNR)
    (map (λ (sample)
           (- (+ (* (random-standard-normal) (noise-level seq SNR)) sample) 0.5))
      seq)))

(define noise-level
  (λ (seq SNR)
    (/ 1.0 (* (signal-power seq) SNR))))

(define signal-power
  (λ (seq)
    (for/fold ((s 0) #:result (/ s 16)) ((v seq))
      (+ s (* v v)))))

(define morse-code-time-series
  (λ (letter)
    (list->tensor
      (add-noise-and-remove-dc
        (embed-to letter-length (dict-ref morse-code letter))))))

(define morse-data-set
  (λ (samples-per-class)
    (let ((N (length morse-code)))
      (for/fold ((s '())) ((m (in-range samples-per-class)))
        (append s (one-set-of-data N))))))

(define one-set-of-data
  (λ (N)
    (for/list ((rib morse-code)
               (i (in-naturals 0)))
      (cons
        (list->tensor
          (add-noise-and-remove-dc
            (embed-to letter-length (cdr rib)) (dB 30)))
        (list->tensor
          (for/list ((j (in-range 0 N)))
            (cond
              ((= i j) 1.0)
              (else 0.0))))))))

(define gen-morse-data-set
  (λ (filename)
    (let ((port (open-output-file filename #:exists 'truncate)))
      (fprintf port "#lang racket~%")
      (pretty-display `(require malted) port)
      (pretty-display `(define signal1->signal2
                         (λ (t)
                           (reshape (append (shape t) '(1)) t))) port)
      (pretty-display `(define morse-ds (list ,@(map unparse (morse-data-set 200)))) port)
      (pretty-display `(define morse-train-xs (signal1->signal2 (list->tensor (map car morse-ds)))) port)
      (pretty-display `(define morse-train-ys (list->tensor (map cdr morse-ds))) port)

      (pretty-display `(define morse-test-ds (list ,@(map unparse (morse-data-set 40)))) port)
      (pretty-display `(define morse-test-xs (signal1->signal2 (list->tensor (map car morse-test-ds)))) port)
      (pretty-display `(define morse-test-ys (list->tensor (map cdr morse-test-ds))) port)

      (pretty-display `(define morse-validate-ds (list ,@(map unparse (morse-data-set 40)))) port)
      (pretty-display `(define morse-validate-xs (signal1->signal2 (list->tensor (map car morse-test-ds)))) port)
      (pretty-display `(define morse-validate-ys (list->tensor (map cdr morse-test-ds))) port)

      (pretty-display `(provide morse-train-xs morse-train-ys
                                morse-test-xs morse-test-ys
                                morse-validate-xs morse-validate-ys) port)
      (close-output-port port))))

(define unparse-tensor
  (λ (t)
    (cond
      ((scalar? t) t)
      (else `(tensor ,@(for/list ([i (tlen t)])
                         (unparse-tensor (tref t i))))))))

(define unparse
  (λ (dp)
    `(cons ,(unparse-tensor (car dp))
           ,(unparse-tensor (cdr dp)))))

(provide gen-morse-data-set)
