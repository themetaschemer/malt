#lang scribble/manual
@title[#:tag "hypers"]{Hyperparameters}

@declare-exporting[malt]
@deftogether[(
@defform[(declare-hyper h)]{}
@defform[(declare-hypers h ...)])]{
Defines @racket[h ...] to be hyperparameters. The names @racket[h ...] and corresponding setter function @racket[set-h! ...] are
automatically provided from the module. The initial values of @racket[h ...] are undefined.
}


@deftogether[(
@defform[(with-hyper ((h v)) expr ...)]
@defform[(with-hypers ((h v) ...) expr ...)])]{
  Temporarily defines the hyperparameters @racket[h ...] the value @racket[v ...] during the evaluation of @racket[expr ...]. When
  the form returns a value, the hyperparameters are reset to their previous values (which may be undefined). Nested @racket[with-hypers]
  forms may be invoked during the invocation of @racket[expr] to temporarily redefine a given hyperparameter to a new value.
}

@defform[(grid-search stop? ((h v ...) ...) expr ...)]{
  Like @racket[with-hypers], but accepts more than one @racket[v] for each hyperparameter @racket[h].
  Sequentially evaluates @racket[expr ...] for each @racket[h] being assigned a value from @racket[v ...]. All combinations
  are tried until @racket[stop?] invoked on the value returned by @racket[(begin expr ...)] returns @racket[#t].
}

@defform[(hypers h ...)]{
  A @racket[provide] sub-form that exports the hyperparameters @racket[h ...]. This @racket[provide] sub-form is only needed
  when re-exporting hypers imported from other modules. By default, @racket[declare-hypers] will export the hyper from the module where
  it was declared.
}
