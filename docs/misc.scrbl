#lang scribble/manual
@title{Models and Accuracy}

@defmodule[malt]

A model is a function that appoximates the data generating function.

@defproc[(model [network-fn (-> (tensor?) (-> theta? tensor?))] [θ theta?]) (-> (tensor?) tensor?)]{
Create a model from a network function @racket[network-fn] and a parameter set θ. The model is a function that returns
an output @racket[tensor?] given and input tensor.
}

@defproc[(accuracy [a-model (-> (tensor?) tensor?)] [xs tensor?] [ys tensor?]) number?]{
Calculates the classification accuracy of the model as a real number between 0.0 and 1.0. @racket[ys] is a tensor of one-hot tensors.
Generates a tensor @italic{r} by applying @racket[model] to @racket[xs]. The shapes of @italic{r} and @racket[ys] must be identical.
Finally returns the number of positions in  @racket[(argmax xs)] and @racket[(argmax (model xs))] that are equal divided by @racket[(tlen ys)]
}