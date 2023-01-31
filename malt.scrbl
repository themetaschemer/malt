#lang scribble/manual
@(require (for-label (for-label (except-in racket + - / * expt exp abs log max sqrt argmax sqr < > = >= <=)
                              "main.rkt")))
@title{Malt - A minimalist deep learning toolkit}
@author{Anurag Mendhekar and Daniel P. Friedman}

Malt is a minimalist deep learning toolkit that is designed to support
the book @italic{The Little Learner}, by Daniel P. Friedman and
Anurag Mendhekar.  The toolkit provides the basics necessary to build
simple deep-learning networks, but more importantly, provides a
framework that can be extended for more complex networks.

The framework provides for tensors, automatic differentiation,
gradient descent, commonly used loss functions, layer functions and
neural network construction tools.

@table-of-contents[]

@include-section["docs/entry-points.scrbl"]
@include-section["docs/list-functions.scrbl"]
@include-section["docs/tensor-functions.scrbl"]
@include-section["docs/extension-functions.scrbl"]
@include-section["docs/autodiff-functions.scrbl"]
@include-section["docs/differentiable-operators.scrbl"]
@include-section["docs/non-differentiable-operators.scrbl"]
@include-section["docs/base-rank-operators.scrbl"]
@include-section["docs/boolean-comparators.scrbl"]
@include-section["docs/tensorized-comparators.scrbl"]
@include-section["docs/hypers.scrbl"]
@include-section["docs/loss.scrbl"]
@include-section["docs/layer.scrbl"]
@include-section["docs/gd.scrbl"]
@include-section["docs/blocks.scrbl"]
@include-section["docs/init.scrbl"]
@include-section["docs/randoms.scrbl"]
@include-section["docs/misc.scrbl"]
@include-section["docs/logging.scrbl"]
