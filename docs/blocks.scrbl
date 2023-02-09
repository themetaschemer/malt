#lang scribble/manual
@title{Building blocks for neural networks}

@declare-exporting[malt]

The type @racket[block?] consists of two fields, a @italic{block function}
and a @italic{block list}.

@defproc[(block [b-fn (-> tensor? (-> (listof tensor?) tensor?))] [block-ls (listof shape?)]) block?]{
  Creates a block from the block function @racket[b-fn] and the block list @racket[block-ls].
}

@defproc[(block-fn [b block?]) (-> tensor? (-> (listof tensor?) tensor?))]{
  Returns the block function from the block @racket[b].
}

@defproc[(block-ls [b block?]) (listof shape?)]{
  Returns the block list from the block @racket[b].
}

@defproc[(stack-blocks [blocks (listof block?)]) block?]{
  Returns a new block where the block function is a composition of the block functions
  of each block in @racket[blocks] in the order they appear @racket[blocks], and the block list is
  created by appending the block lists of each block in @racket[blocks] in the order they appear in @racket[blocks].
}
