#lang scribble/manual
@title{Tensorized comparison functions}

These functions compare two tensors of rank 1 and the same shape
and return a tensor where an element is 1.0 if the corresponding
scalars in the two argument tensors would return @racket[#t] when
compared with the given scalar comparison function.

@defmodule*[(malt malt/base malt/base-no-duals malt/base-no-overrides malt/base-no-duals-no-overrides malt/learner malt/flat-tensors malt/nested-tensors)]

@defproc[(=-1 (t0 tensor?) (t1 tensor?)) tensor?]{
 Compare the scalars in @racket[t0] and @racket[t1] with @racket[=-0-0]
}

@defproc[(<-1 (t0 tensor?) (t1 tensor?)) tensor?]{
 Compare the scalars in @racket[t0] and @racket[t1] with @racket[<-0-0]
}

@defproc[(<=-1 (t0 tensor?) (t1 tensor?)) tensor?]{
 Compare the scalars in @racket[t0] and @racket[t1] with @racket[<=-0-0]
}

@defproc[(>-1 (t0 tensor?) (t1 tensor?)) tensor?]{
 Compare the scalars in @racket[t0] and @racket[t1] with @racket[>-0-0]
}

@defproc[(>=-1 (t0 tensor?) (t1 tensor?)) tensor?]{
 Compare the scalars in @racket[t0] and @racket[t1] with @racket[>=-0-0]
}
