Descriptions of package entry points

1. main.rkt
   -- The whole malt package, ready to be used to build examples

2. base.rkt
   -- Tensors, autodiff and tools.
       -- no machine learning functions. Useful for those who
          are trying to follow along in the book starting with
          gradient descent.

3. base-no-duals.rkt
   -- Tensors and tools only, no autodiff
       -- For people trying to build autodiff from the appendices
       -- Also good for using non-differentiable extended functions

--------------------------------
Tensor representations. The code in this package supports two types
of tensor representations:

1. Nested Tensors

and

2. Flat Tensors.

The default is Flat Tensors for 1, 2 and 3 above.
--------------------------------
In the "Developer Version", the Makefile can be used to switch between different representations.

--------------------------------
These are for internal usage within malted.

6. impl.rkt
   -- Loads the specified implementation of tensors

7. impl-no-duals.rkt
   -- Loads the specified implementation of tensors

--------------------------------
Collections and their loaders.

Flat tensors
    -- Tensors, extended functions, auto-diff using the flat representation
    -- loaded using flat-tensors.rkt

Nested tensors
    -- Tensors, extended functions, auto-diff using the flat representation
    -- loaded using flat-tensors.rkt

Tools
    -- Tools that are abstracted out:
        Hyperparameters, Normally distributed Random numbers, Logging
    -- Loaded using tools.rkt, depends upon impl.rkt

Malted
    -- Layer functions, Loss, GD, Dense/Recu Layers, Blocks, Initialization
    -- loaded using malted.rkt, depends upon impl.rkt and impl-no-duals.rkt

--------------------------------
Future tensor implementations to be supported

f32flat tensors

f32flat with GPU support tensors
