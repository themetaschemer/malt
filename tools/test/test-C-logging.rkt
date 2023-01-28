(module+ test
  (require rackunit)

  (define test-event-loss
    (vector 'info "" '(loss 31.2) 'malt))

  (define test-port
    (λ ()
      (open-output-string "")))

  (define test-non-event-loss
    (vector 'info "message" '() 'malt))


  (let ((tp (test-port)))
    (check-false (parameterize ([current-error-port tp])
                   (malt-log-handler test-non-event-loss)))
    (check-equal? (get-output-string tp) "[malt] info: message\n()\n"))

  (let ((tp (test-port)))
    (parameterize ([current-error-port tp])
      (print-average 33 79))
    (check-regexp-match
     #rx"\\(80 33\\) \\[Memory: [0-9]+\\]\\[Window size 6\\]\n"
     (get-output-string tp) #f))

  ;; With scalar data
  (let ((tp (test-port)))
    (parameterize ([current-error-port tp])
      (let ((st (start-logging 20)))
        (for ([i (in-range 40)])
          (record 'loss (exact->inexact i)))
        (sleep 1)
        (kill-thread st)))
    (check-regexp-match
     #rx"\\(20 16.5\\) \\[Memory: [0-9]+\\]\\[Window size 6\\]\n\\(40 36.5\\) \\[Memory: [0-9]+\\]\\[Window size 6\\]\n"
     (get-output-string tp)))

  ;; With tensor data
  (let ((tp (test-port)))
    (parameterize ([current-error-port tp])
      (let ((st (start-logging 20)))
        (for ([i (in-range 40)])
          (record 'loss (tensor (exact->inexact i) (exact->inexact i))))
        (sleep 1)
        (kill-thread st)))
    (check-regexp-match
     #rx"\\(20 16.5\\) \\[Memory: [0-9]+\\]\\[Window size 6\\]\n\\(40 36.5\\) \\[Memory: [0-9]+\\]\\[Window size 6\\]\n"
     (get-output-string tp)))

  )
