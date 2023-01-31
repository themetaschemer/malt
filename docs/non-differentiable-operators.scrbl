#lang scribble/manual
@title[#:tag "non-diffable-fns"]{Non-differentiable extended numerical functions}

Non-differentiable versions of the functions in @secref{diffable-fns}. These functions
don't accept duals or return duals. They are primarily used to seek better memory
performance where automatic differentiation is not required.

@defmodule[malt]


@defproc[(+-ρ [t0 tensor?] [t1 tensor?]) tensor?]{
Adds @racket[t0] and @racket[t1] based on binary extended function rules.
}

@defproc[(--ρ [t0 tensor?] [t1 tensor?]) tensor?]{
Subtracts @racket[t0] and @racket[t1] based on binary extended function rules.
}

@defproc[(*-ρ [t0 tensor?] [t1 tensor?]) tensor?]{
Multiplies @racket[t0] and @racket[t1] based on binary extended function rules.
}

@defproc[(/-ρ [t0 tensor?] [t1 tensor?]) tensor?]{
Divides @racket[t0] into @racket[t1] based on binary extended function rules.
}

@defproc[(expt-ρ [t0 tensor?] [t1 tensor?]) tensor?]{
Computes @racket[t0] raised to the power of @racket[t1] based on binary extended function rules.
}

@defproc[(exp-ρ [t0 tensor?]) tensor?]{
Computes @italic{e} raised to the power of @racket[t0] based on unary extended function rules.
}

@defproc[(log-ρ [t0 tensor?]) tensor?]{
Computes logarithm of @racket[t0] based on unary extended function rules.
}

@defproc[(abs-ρ [t0 tensor?]) tensor?]{
Computes absolute value of @racket[t0] based on unary extended function rules.
}

@defproc[(sqrt-ρ [t0 tensor?]) tensor?]{
Computes the square root of @racket[t0] based on unary extended function rules.
}

@defproc[(sqr-ρ [t0 tensor?]) tensor?]{
Computes the square of @racket[t0] based on unary extended function rules.
}

@defproc[(sum-ρ [t0 tensor?]) tensor?]{
If t0 is a tensor of rank 1, return the sum of all the @racket[scalar?] elements in @racket[t0].

When @racket[t0] is of rank higher than 1, the rules of unary extended functions apply.
}

@defproc[(sum-cols-ρ [t0 tensor?]) tensor?]{
If t0 is a tensor of rank 2, return the sum of all the @racket[scalar?] elements in @racket[t0], calculated using
the extended addition function @racket[+-ρ].

When @racket[t0] is of rank higher than 2, the rules of unary extended functions apply.

When @racket[t0] is of rank 1, this function is the same as @racket[sum].

The function is undefined if @racket[t0] is a scalar.
}

@defproc[(*-2-1-ρ [t0 tensor?] [t1 tensor?]) tensor?]{
When @racket[t0] is a tensor of shape @racket[(list m n)] and @racket[t1] is a tensor of shape @racket[(list n)],
returns a tensor @italic{r} of shape @racket[(list m n)] where the @racket[m] elements of @italic{r} are formed
by multiplying each element of @racket[t0] with @racket[t1] using the extended multiplication function @racket[*-ρ].

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the rules of binary extended
functions apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.
}

@defproc[(argmax-ρ [t0 tensor?]) tensor?]{
When @racket[t0] is a tensor of rank 1, returns the index of the highest element in @racket[t0].

When @racket[t0] is of rank higher than 1, the rules of unary extended functions apply.

The function is undefined when @racket[t0] is of rank less than 1.
}

@defproc[(max-ρ [t0 tensor?]) tensor?]{
When @racket[t0] is a tensor of rank 1, returns the highest element in @racket[t0].

When @racket[t0] is of rank higher than 1, the rules of unary extended functions apply.

The function is undefined when @racket[t0] is of rank less than 1.
}

@defproc[(dot-product-ρ [t0 tensor?] [t1 tensor?]) tensor?]{
When @racket[t0] and @racket[t1] are tensors of rank 1 and the same shape,
returns a scalar @italic{s} formed
by multiplying each element of @racket[t0] with the corresponding element in @racket[t1] and summing the results.

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the rules of binary extended
functions apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.
}

@defproc[(dot-product-2-1-ρ [t0 tensor?] [t1 tensor?]) tensor?]{
When @racket[t0] is a tensor of shape @racket[(list m n)] and @racket[t1] is a tensor of shape @racket[(list n)],
returns a tensor @italic{r} of shape @racket[(list m)] where the @racket[m] elements of @italic{r} are formed
by from the @racket[dot-product] of each element of @racket[t0] with @racket[t1].

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the rules of binary extended
functions apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.
}


@defproc[(correlate-ρ [filter-bank tensor?] [signal tensor?]) tensor?]{
When @racket[filter-bank] is a tensor of shape @racket[(list b m d)] and @racket[signal] is a tensor of shape @racket[(list n d)],
returns a tensor @italic{r} of shape @racket[(list n b)] which is the correlation of the @racket[filter-bank] with the @racket[signal].

When the ranks of @racket[filter-bank] and @racket[signal] are higher than 3 and 2 respectively, the rules of binary extended
functions apply.
}
