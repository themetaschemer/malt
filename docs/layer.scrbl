#lang scribble/manual
@title{Layer functions}

@defmodule[malt]

@section{Single layer functions}

These functions accept an input tensor, a @racket[θ] and return an output tensor. These functions are
all of the type [target-fn?] (see @secref{loss-fns}).

@defproc[((line [t tensor?]) [θ theta?]) tensor?]{
  Implements the linear combination @codeblock{
    (+ (* (ref θ 0) t)
       (ref θ 1))
  }
}

@defproc[((quad [t tensor?]) [θ theta?]) tensor?]{
  Implements the polynomial combination @codeblock{
    (+ (* (ref θ 0) (sqr t))
       (* (ref θ 1) t)
       (ref θ 2))
  }
}

@defproc*[((((plane [t tensor?]) [θ theta?]) tensor?)
           (((linear-1-1 [t tensor?]) [θ theta?]) tensor?))]{
  Implements the polynomial combination @codeblock{
  (+ (dot-product (ref θ 0) t) (ref θ 1))
  }
}

@defproc[((softmax [t tensor?]) [θ theta?]) tensor?]{
  Implements the softmax function @codeblock{
  (let ((z (- t (max t))))
    (let ((expz (exp z)))
      (/ expz (sum expz))))
  }
}

@defproc[((relu [t tensor?]) [θ theta?]) tensor?]{
  Implements the ReLU function @codeblock{
  (rectify ((linear t) theta))
  }
}

@defproc[((relu [t tensor?]) [θ theta?]) tensor?]{
  Implements the ReLU function @codeblock{
  (rectify ((linear t) theta))
  }
}

@defproc[((corr [t tensor?]) [θ theta?]) tensor?]{
  Implements the biased correlation function @codeblock{
  (+ (correlate (ref θ 0) t) (ref θ 1))
  }
}

@defproc[((recu [t tensor?]) [θ theta?]) tensor?]{
  Implements the rectified 1D-convolution function @codeblock{
  (rectify ((corr t) theta))
  }
}

@defproc*[((((signal-avg [t tensor?]) [θ theta?]) tensor?)
           (((avg-cols [t tensor?]) [θ theta?]) tensor?))]{
  Implements the averaging of the rank 1 elements of a tensor @codeblock{
  (let ((num-segments (ref (refr (shape t) (- (rank t) 2)) 0)))
    (/ (sum-cols t) num-segments))
  }
}

@section{Deep layer functions}

These functions create a stacked composition of layer functions by providing
the depth @racket[k] of composition. The composition is also a layer function.

@defproc[(((k-relu [k natural?]) [t tensor?]) [θ theta?]) tensor?]{
  Implements a composition of @racket[k] ReLU functions. Can be used to
  implement a neural network exclusively made up of dense layers.
}

@defproc[(((k-recu [k natural?]) [t tensor?]) [θ theta?]) tensor?]{
  Implements a composition of @racket[k] rectified 1D-convolution (@racket[recu]) functions . Can be used to
  implement a neural network exclusively made up of @racket[recu] layers.
}
