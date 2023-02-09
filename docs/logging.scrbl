#lang scribble/manual
@title{Logging}

@declare-exporting[malt]

Functions for logging information and reporting moving averages of some types of quantities.

@defproc[(start-logging) void?]{
 Begin log reporting. This function starts a thread with a @racket[log-receiver] for @racket[malt] specific
 logging.

 This function must be invoked before any logs are displayed or averaged.
}

@defproc[(record [name symbol?] [qty tensor?]) tensor?]{
  Records @racket[qty] as another datapoint for @racket[name]. If @racket[name] supports it,
  a moving average of 6 data points will be logged in a message every 20 calls to record.

  Currently the only @racket[name] that supports moving average calculation is @racket['loss].
}

@defproc[(with-recording [loss-fn loss-function?]) loss-function?]{
  Returns a loss-function that @racket[record]s the loss produced by @racket[loss-fn], using
  the @racket[name] @racket['loss].
}

@deftogether[(
@defform*[((log-malt-fatal string-expr)
           (log-malt-fatal format-string-expr v ...))]
@defform*[((log-malt-error string-expr)
           (log-malt-error format-string-expr v ...))]
@defform*[((log-malt-warning string-expr)
           (log-malt-warning format-string-expr v ...))]
@defform*[((log-malt-info string-expr)
           (log-malt-info format-string-expr v ...))]
@defform*[((log-malt-debug string-expr)
           (log-malt-debug format-string-expr v ...))])]{
  Log a message at the associated level. Follows conventions of the default @hyperlink["https://docs.racket-lang.org/reference/logging.html"]{Racket logger}.
}
