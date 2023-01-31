#lang scribble/manual
@title[#:tag "diffable-fns"]{Differentiable extended numerical functions}

@defmodule[malt]

The gradients of functions constructed from the following functions as primitives can be
computed using @racket[∇] @racket[∇¹] or @racket[gradient-of].

@defproc[(+ [t0 tensor?] [t1 tensor?]) tensor?]{
Adds @racket[t0] and @racket[t1] based on binary extended function rules.

The same as @racket[(ext2 +-0-0 0 0)].
}

@defproc[(- [t0 tensor?] [t1 tensor?]) tensor?]{
Subtracts @racket[t0] and @racket[t1] based on binary extended function rules.

The same as @racket[(ext2 --0-0 0 0)].
}

@defproc[(* [t0 tensor?] [t1 tensor?]) tensor?]{
Multiplies @racket[t0] and @racket[t1] based on binary extended function rules.

The same as @racket[(ext2 *-0-0 0 0)].
}

@defproc[(/ [t0 tensor?] [t1 tensor?]) tensor?]{
Divides @racket[t0] into @racket[t1] based on binary extended function rules.

The same as @racket[(ext2 /-0-0 0 0)].
}

@defproc[(expt [t0 tensor?] [t1 tensor?]) tensor?]{
Computes @racket[t0] raised to the power of @racket[t1] based on binary extended function rules.

The same as @racket[(ext2 expt-0-0 0 0)].
}

@defproc[(exp [t0 tensor?]) tensor?]{
Computes @italic{e} raised to the power of @racket[t0] based on unary extended function rules.

The same as @racket[(ext1 exp-0 0)].
}

@defproc[(log [t0 tensor?]) tensor?]{
Computes logarithm of @racket[t0] based on unary extended function rules.

The same as @racket[(ext1 log-0 0)].
}

@defproc[(abs [t0 tensor?]) tensor?]{
Computes absolute value of @racket[t0] based on unary extended function rules.

The same as @racket[(ext1 abs-0 0)].
}

@defproc[(sqrt [t0 tensor?]) tensor?]{
Computes the square root of @racket[t0] based on unary extended function rules.

The same as @racket[(expt t0 1/2)].
}

@defproc[(sqr [t0 tensor?]) tensor?]{
Computes the square of @racket[t0] based on unary extended function rules.

The same as @racket[(* t0 t0)].
}

@defproc[(sum [t0 tensor?]) tensor?]{
If t0 is a tensor of rank 1, return the sum of all the @racket[scalar?] elements in @racket[t0].

When @racket[t0] is of rank higher than 1, the rules of unary extended functions apply.

The same as @racket[(ext1 sum-1 1)].
}

@defproc[(sum-cols [t0 tensor?]) tensor?]{
If t0 is a tensor of rank 2, return the sum of all the @racket[scalar?] elements in @racket[t0], calculated using
the extended addition function @racket[+].

When @racket[t0] is of rank higher than 2, the rules of unary extended functions apply.

When @racket[t0] is of rank 1, this function is the same as @racket[sum].

The function is undefined if @racket[t0] is a scalar.

The same as @racket[(ext1 sum-1 2)].
}

@defproc[(*-2-1 [t0 tensor?] [t1 tensor?]) tensor?]{
When @racket[t0] is a tensor of shape @racket[(list m n)] and @racket[t1] is a tensor of shape @racket[(list n)],
returns a tensor @italic{r} of shape @racket[(list m n)] where the @racket[m] elements of @italic{r} are formed
by multiplying each element of @racket[t0] with @racket[t1] using the extended multiplication function @racket[*].

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the rules of binary extended
functions apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.

The same as @racket[(ext2 * 2 1)].
}

@defproc[(argmax [t0 tensor?]) tensor?]{
When @racket[t0] is a tensor of rank 1, returns the index of the highest element in @racket[t0].

When @racket[t0] is of rank higher than 1, the rules of unary extended functions apply.

The function is undefined when @racket[t0] is of rank less than 1.

The same as @racket[(ext1 argmax-1 1)].
}

@defproc[(max [t0 tensor?]) tensor?]{
When @racket[t0] is a tensor of rank 1, returns the highest element in @racket[t0].

When @racket[t0] is of rank higher than 1, the rules of unary extended functions apply.

The function is undefined when @racket[t0] is of rank less than 1.

The same as @racket[(ext1 max-1 1)].
}

@defproc[(dot-product [t0 tensor?] [t1 tensor?]) tensor?]{
When @racket[t0] and @racket[t1] are tensors of rank 1 and the same shape,
returns a scalar @italic{s} formed
by multiplying each element of @racket[t0] with the corresponding element in @racket[t1] and summing the results.

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the rules of binary extended
functions apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.

The same as @racket[(sum (* t0 t1))].
}

@defproc[(dot-product-2-1 [t0 tensor?] [t1 tensor?]) tensor?]{
When @racket[t0] is a tensor of shape @racket[(list m n)] and @racket[t1] is a tensor of shape @racket[(list n)],
returns a tensor @italic{r} of shape @racket[(list m)] where the @racket[m] elements of @italic{r} are formed
by from the @racket[dot-product] of each element of @racket[t0] with @racket[t1].

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the rules of binary extended
functions apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.

The same as @racket[(sum (*-2-1 t0 t1))].
}


@defproc[(correlate [filter-bank tensor?] [signal tensor?]) tensor?]{
When @racket[filter-bank] is a tensor of shape @racket[(list b m d)] and @racket[signal] is a tensor of shape @racket[(list n d)],
returns a tensor @italic{r} of shape @racket[(list n b)] which is the correlation of the @racket[filter-bank] with the @racket[signal].

When the ranks of @racket[filter-bank] and @racket[signal] are higher than 3 and 2 respectively, the rules of binary extended
functions apply.
}
