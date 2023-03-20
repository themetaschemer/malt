#lang scribble/manual
@title[#:tag "set-impl"]{Setting tensor implementations}

@defmodule[malt/set-impl]

@defproc[(set-impl [impl (one-of/c 'learner 'nested-tensors 'flat-tensors)]) void]{
  Sets the tensor implementation to the named implementation
  (one of @racket['learner], @racket['nested-tensors], or @racket['flat-tensors]).

  The change in implementation will take effect the next time Racket process is re-started.
  The most convenient way to switch implementations is to do it from the shell command line. For example,
  to set the implementation to @racket['flat-tensors].
  @codeblock{
  $ racket -e "(require malt/set-impl) (set-impl 'flat-tensors)"
  }
}
