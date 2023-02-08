#lang scribble/manual
@(require (for-label (for-label (except-in racket + - / * expt exp abs log max sqrt argmax sqr < > = >= <=)
                              "main.rkt")))
@title{Malt - A minimalist deep learning toolkit}
@author{Anurag Mendhekar and Daniel P. Friedman}

Malt is a minimalist deep learning toolkit that is designed to support
the book @italic{The Little Learner}, by Daniel P. Friedman and
Anurag Mendhekar.

The framework provides for tensors, automatic differentiation,
gradient descent, commonly used loss functions, layer functions and
neural network construction tools.

While it has started off as a pedagogical tool, it is designed with
the future in mind and we are seeking fellow enthusiasts who would be
interested in making it production worthy.

@table-of-contents[]

@include-section["docs/overview.scrbl"]
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
@include-section["docs/gd.scrbl"]
@include-section["docs/layer.scrbl"]
@include-section["docs/loss.scrbl"]
@include-section["docs/blocks.scrbl"]
@include-section["docs/init.scrbl"]
@include-section["docs/randoms.scrbl"]
@include-section["docs/misc.scrbl"]
@include-section["docs/logging.scrbl"]
@include-section["docs/helpers.scrbl"]
