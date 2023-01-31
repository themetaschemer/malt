#lang scribble/manual
@title{Boolean comparison functions}

@defmodule[malt]

These functions are used for comparison between @racket[scalar?]s.

@defproc[(=-0-0 (a scalar?) (b scalar?)) boolean?]{
 Returns #t if @racket[(ρ a)] is equal to @racket[(ρ b)].
}

@defproc[(= (a scalar?) (b scalar?)) boolean?]{
 Another name for @racket[=-0-0].
}

@defproc[(<-0-0 (a scalar?) (b scalar?)) boolean?]{
 Returns #t if @racket[(ρ a)] is less than @racket[(ρ b)].
}

@defproc[(< (a scalar?) (b scalar?)) boolean?]{
 Another name for @racket[<-0-0].
}

@defproc[(>-0-0 (a scalar?) (b scalar?)) boolean?]{
 Returns #t if @racket[(ρ a)] is greater than @racket[(ρ b)].
}

@defproc[(> (a scalar?) (b scalar?)) boolean?]{
 Another name for @racket[>-0-0].
}

@defproc[(<=-0-0 (a scalar?) (b scalar?)) boolean?]{
 Returns #t if @racket[(ρ a)] is less than or equal to @racket[(ρ b)].
}

@defproc[(<= (a scalar?) (b scalar?)) boolean?]{
 Another name for @racket[<=-0-0].
}

@defproc[(>=-0-0 (a scalar?) (b scalar?)) boolean?]{
 Returns #t if @racket[(ρ a)] is greater than or equal to @racket[(ρ b)].
}

@defproc[(>= (a scalar?) (b scalar?)) boolean?]{
 Another name for @racket[>=-0-0].
}
}
