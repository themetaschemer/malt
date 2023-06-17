#lang scribble/manual
@title[#:tag "diffable-fns"]{Differentiable extended numerical functions}

@declare-exporting[malt]

The gradients of functions constructed from the following functions as primitives can be
computed using @racket[∇], @racket[∇¹] or @racket[gradient-of].

@section[#:tag "overridden"]{Extended operators}
@defproc*[([(+ [t0 tensor?] [t1 tensor?]) tensor?]
           [(d+ [t0 tensor?] [t1 tensor?]) tensor?])]{
Adds @racket[t0] and @racket[t1] based on @secref{binary-rules}.

The same as @racket[(ext2 +-0-0 0 0)].

When using @racket[malt/no-overrides], @racket[d+] should be used.
}

@defproc*[([(- [t0 tensor?] [t1 tensor?]) tensor?]
           [(d- [t0 tensor?] [t1 tensor?]) tensor?])]{
Subtracts @racket[t0] and @racket[t1] based on @secref{binary-rules}.

The same as @racket[(ext2 --0-0 0 0)].

When using @racket[malt/no-overrides], @racket[d-] should be used.
}

@defproc*[([(* [t0 tensor?] [t1 tensor?]) tensor?]
           [(d* [t0 tensor?] [t1 tensor?]) tensor?])]{
Multiplies @racket[t0] and @racket[t1] based on @secref{binary-rules}.

The same as @racket[(ext2 *-0-0 0 0)].

When using @racket[malt/no-overrides], @racket[d-*] should be used.
}

@defproc*[([(/ [t0 tensor?] [t1 tensor?]) tensor?]
           [(d/ [t0 tensor?] [t1 tensor?]) tensor?])]{
Divides @racket[t0] into @racket[t1] based on @secref{binary-rules}.

The same as @racket[(ext2 /-0-0 0 0)].

When using @racket[malt/no-overrides], @racket[d-/] should be used.
}

@defproc*[([(expt [t0 tensor?] [t1 tensor?]) tensor?]
           [(d-expt [t0 tensor?] [t1 tensor?]) tensor?])]{
Computes @racket[t0] raised to the power of @racket[t1] based on @secref{binary-rules}.

The same as @racket[(ext2 expt-0-0 0 0)].

When using @racket[malt/no-overrides], @racket[d-expt] should be used.
}

@defproc*[([(exp [t tensor?]) tensor?]
           [(d-exp [t tensor?]) tensor?])]{
Computes @italic{e} raised to the power of @racket[t] based on @secref{unary-rules}.

The same as @racket[(ext1 exp-0 0)].

When using @racket[malt/no-overrides], @racket[d-exp] should be used.
}

@defproc*[([(log [t tensor?]) tensor?]
           [(d-log [t tensor?]) tensor?])]{
Computes logarithm of @racket[t] based on @secref{unary-rules}.

The same as @racket[(ext1 log-0 0)].

When using @racket[malt/no-overrides], @racket[d-log] should be used.
}

@defproc*[([(abs [t tensor?]) tensor?]
           [(d-abs [t tensor?]) tensor?])]{
Computes absolute value of @racket[t] based on @secref{unary-rules}.

The same as @racket[(ext1 abs-0 0)].

When using @racket[malt/no-overrides], @racket[d-abs] should be used.
}

@defproc*[([(sqrt [t tensor?]) tensor?]
           [(d-sqrt [t tensor?]) tensor?])]{
Computes the square root of @racket[t] based on  @secref{unary-rules}.

The same as @racket[(expt t 1/2)].

When using @racket[malt/no-overrides], @racket[d-sqrt] should be used.
}

@defproc*[([(sqr [t tensor?]) tensor?]
           [(d-sqr [t tensor?]) tensor?])]{
Computes the square of @racket[t] based on @secref{unary-rules}.

The same as @racket[(* t t)].

When using @racket[malt/no-overrides], @racket[d-sqr] should be used.
}

@defproc*[([(sum [t tensor?]) tensor?]
           [(d-sum [t tensor?]) tensor?])]{
If @racket[t] is a tensor of rank 1, return the sum of all the @racket[scalar?] elements in @racket[t].

When @racket[t] is of rank higher than 1, the @secref{unary-rules} apply.

The same as @racket[(ext1 sum-1 1)].

When using @racket[malt/no-overrides], @racket[d-sum] should be used.
}

@defproc*[([(sum-cols [t tensor?]) tensor?]
           [(d-sum-cols [t tensor?]) tensor?])]{
If @racket[t] is a tensor of rank 2, return the sum of all the @racket[scalar?] elements in @racket[t], calculated using
the extended addition function @racket[+].

When @racket[t] is of rank higher than 2, the @secref{unary-rules} apply.

When @racket[t] is of rank 1, this function is the same as @racket[sum].

The function is undefined if @racket[t] is a scalar.

The same as @racket[(ext1 sum-1 2)].

When using @racket[malt/no-overrides], @racket[d-sum-cols] should be used.
}

@defproc*[([(*-2-1 [t0 tensor?] [t1 tensor?]) tensor?]
           [(d*-2-1 [t0 tensor?] [t1 tensor?]) tensor?])]{
When @racket[t0] is a tensor of shape @racket[(list m n)] and @racket[t1] is a tensor of shape @racket[(list n)],
returns a tensor @italic{r} of shape @racket[(list m n)] where the @racket[m] elements of @italic{r} are formed
by multiplying each element of @racket[t0] with @racket[t1] using the extended multiplication function @racket[*].

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the @secref{binary-rules} apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.

The same as @racket[(ext2 * 2 1)].

When using @racket[malt/no-overrides], @racket[d*-2-1] should be used.
}

@defproc*[([(argmax [t tensor?]) tensor?]
           [(d-argmax [t tensor?]) tensor?])]{
When @racket[t] is a tensor of rank 1, returns the index of the highest element in @racket[t].

When @racket[t] is of rank higher than 1, the @secref{unary-rules} apply.

The function is undefined when @racket[t] is of rank less than 1.

The same as @racket[(ext1 argmax-1 1)].

When using @racket[malt/no-overrides], @racket[d-argmax] should be used.
}

@defproc*[([(max [t tensor?]) tensor?]
           [(d-max [t tensor?]) tensor?])]{
When @racket[t] is a tensor of rank 1, returns the highest element in @racket[t].

When @racket[t] is of rank higher than 1, the @secref{unary-rules} apply.

The function is undefined when @racket[t] is of rank less than 1.

The same as @racket[(ext1 max-1 1)].

When using @racket[malt/no-overrides], @racket[d-max] should be used.
}

@defproc*[([(concat [t tensor?] [u tensor?]) tensor?]
           [(d-concat [t tensor?] [u tensor?]) tensor?])]{
When @racket[t] and @racket[u] are tensors of rank 1, returns a tensor that is the concatenation of @racket[t] and @racket[u].

Otherwise, the @secref{binary-rules} apply.

The function is undefined when @racket[t] or @racket[u] is of rank less than 1.

The same as @racket[(ext2 concat-1-1 1 1)].

When using @racket[malt/no-overrides], @racket[d-concat] should be used.
}

@defproc*[([(concat-n [n positive-integer?]) (-> ([t tensor?] [u tensor?]) tensor?)]
           [(d-concat-n [n positive-integer?]) (-> ([t tensor?] [u tensor?]) tensor?)])]{
When @racket[t] and @racket[u] are tensors of rank @racket[n], returns a tensor that is the concatenation of @racket[t] and @racket[u].

Otherwise, the @secref{binary-rules} apply.

The function is undefined when @racket[t] or @racket[u] is of rank less than @racket[n].

When using @racket[malt/no-overrides], @racket[d-concat-n] should be used.
}

@defproc[(dot-product [t0 tensor?] [t1 tensor?]) tensor?]{
When @racket[t0] and @racket[t1] are tensors of rank 1 and the same shape,
returns a scalar @italic{s} formed
by multiplying each element of @racket[t0] with the corresponding element in @racket[t1] and summing the results.

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the @secref{binary-rules} apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.

The same as @racket[(sum (* t0 t1))].
}

@defproc[(dot-product-2-1 [t0 tensor?] [t1 tensor?]) tensor?]{
When @racket[t0] is a tensor of shape @racket[(list m n)] and @racket[t1] is a tensor of shape @racket[(list n)],
returns a tensor @italic{r} of shape @racket[(list m)] where the @racket[m] elements of @italic{r} are formed
by the @racket[dot-product] of each element of @racket[t0] with @racket[t1].

When the ranks of @racket[t0] and @racket[t1] are higher than 2 and 1 respectively, the @secref{binary-rules} apply.

The function is undefined if @racket[t0] has rank less than 2 and @racket[t1] has rank less than 1.

The same as @racket[(sum (*-2-1 t0 t1))].
}


@defproc[(correlate [filter-bank tensor?] [signal tensor?]) tensor?]{
When @racket[filter-bank] is a tensor of shape @racket[(list b m d)] and @racket[signal] is a tensor of shape @racket[(list n d)],
returns a tensor @italic{r} of shape @racket[(list n b)] which is the correlation of the @racket[filter-bank] with the @racket[signal].

When the ranks of @racket[filter-bank] and @racket[signal] are higher than 3 and 2 respectively, the @secref{binary-rules} apply.
}
