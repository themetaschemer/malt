#lang scribble/manual
@title{He Initialization}

@defmodule[malt]

These functions are used in the setting the initial values in a given @racket[theta] based
on the shapes of the tensors desired in the @racket[theta].

@defproc[(init-theta [θ-shapes (listof shape?)]) (listof tensor?)]{
  Returns a list of tensors with shapes defined by @racket[θ-shapes] with the following intialization rules.
  @itemlist[
  @item{Tensors of rank 1 are initialized to contain only 0.0}
  @item{Tensors of rank 2 are initialized to random numbers drawn from a normal distribution with a mean of 0.0
        and a variance of @racket[(/ 2 fan-in)] where fan-in is the last member of the shape.}
  @item{Tensors of rank 3 are initialized to random numbers drawn from a normal distribution with a mean of 0.0
        and a variance of @racket[(/ 2 fan-in)] where fan-in is the product of the last two members of the shape.}
  ]
}

@defproc[(init-shape [s shape?]) tensor?]{
  Returns a tensor of shape @racket[s] according to the rules described in @racket[init-theta].
}

@defproc[(zero-tensor [s shape?]) tensor?]{
  Returns a tensor of shape @racket[s] with only 0.0s as nested scalars in it.
}