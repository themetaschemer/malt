#lang scribble/manual
@title[#:tag "base-rank-differentiable-functions"]{Base-rank (non-extended) differentiable functions}

@defmodule{malt}

The following functions are non-extended functions defined on tensors of the specified base-rank. No extended function rules
apply to these functions. They can be arguments to @racket[ext1] (if unary) or @racket[ext2] (if binary). These functions
are differentiable.

@defproc[(+-0-0 [a scalar?] [b scalar?]) scalar?]{
  Returns the differentiable addition of @racket[a] and @racket[b].
}

@defproc[(--0-0 [a scalar?] [b scalar?]) scalar?]{
  Returns the differentiable difference between @racket[a] and @racket[b].
}

@defproc[(*-0-0 [a scalar?] [b scalar?]) scalar?]{
  Returns the differentiable product of @racket[a] and @racket[b].
}

@defproc[(/-0-0 [a scalar?] [b scalar?]) scalar?]{
  Returns the differentiable division of @racket[a] and @racket[b].
}

@defproc[(expt-0-0 [a scalar?] [b scalar?]) scalar?]{
  Returns the exponentiation of @racket[a] to the power @racket[b].
}

@defproc[(exp-0 [a scalar?]) scalar?]{
  Returns the exponentiation of @italic{e} to the power @racket[a].
}

@defproc[(log-0 [a scalar?]) scalar?]{
  Returns the natural (to the base @italic{e}) logarithm @racket[a].
}

@defproc[(abs-0 [a scalar?]) scalar?]{
  Returns absolute value of @racket[a].
}

@defproc[(rectify-0 [a scalar?]) scalar?]{
  Returns @racket[0.0] if @racket[a] is less than @racket[0], or @racket[a] otherwise.
}

@defproc[(sum-1 [a tensor?]) scalar?] {
  Here @racket[a] must be a tensor of rank 1. Returns the sum of all the scalars in @racket[a].
}

@defproc[(argmax-1 [a tensor?]) scalar?] {
  Here @racket[a] must be a tensor of rank 1. Returns the index of the highest scalar in @racket[a].
}

@defproc[(max-1 [a tensor?]) scalar?] {
  Here @racket[a] must be a tensor of rank 1. Returns the highest scalar in @racket[a].
}
