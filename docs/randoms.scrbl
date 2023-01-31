#lang scribble/manual
@title{Random number functions}

@defmodule[malt]

@defproc[(random-normal [mu scalar?] [sigma scalar?]) scalar?]{
 Returns a random number sampled from the normal distribution with a mean of @racket[mu] and a standard deviation of @racket[sigma].
}

@defproc[(random-standar-normal) scalar?]{
 Returns a random number sampled from the normal distribution with a mean of @racket[0.0] and a standard deviation of @racket[1.0].
}

@defproc[(random-tensor [mean scalar?] [variance scalar?] [s (listof natural?)]) tensor?]{
 Generates a tensor of shape @racket[s] where the scalars are drawn from a normal distribution with a mean given by @racket[mean]
 and the standard deviation given by @racket[(sqrt variance)].
}