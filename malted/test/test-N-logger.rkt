(module+ test
  (require rackunit)

  (define r1 (new-rotbuf))

  (push! r1 3.0)
  (push! r1 4.0)
  (check-equal? (avg r1) 3.5)

  (define r2 (new-rotbuf))
  (for ([i (in-range 12)])
    (push! r2 i))
  (check-equal? (avg r2)
                (/ (+ 2 3 4 5 6 7 8 9 10 11) 10.0))

  (define w1 (new-window 'loss))

  (add! w1 3.0)
  (add! w1 4.0)

  (define msg1
    (parameterize ([current-error-port (open-output-string)])
      (print-average 'loss (window-buf w1))
      (get-output-string (current-error-port))))

  (check-equal? (read (open-input-string msg1)) '(2 3.5))

  (define w2 (new-window 'loss2))
  (define msg2
    (parameterize ([current-error-port (open-output-string)])
      (for ([i (in-range 22)])
        (add! w2 i))
      (get-output-string (current-error-port))))
  (check-equal? (read (open-input-string msg2)) '(21 15.5))
  )
