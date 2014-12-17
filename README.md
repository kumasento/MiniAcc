
#Explanation of version 1:

Let's start with `addZipWith`:

    addZipWith :: ArrayT -> ArrayT -> AccT
    addZipWith x y = 
        let x' = use x
            y' = use y
        in 
        AccAST.zipWith Add x' y'

Here're 2 things:

1. `addZipWith` is not a traditional Haskell fuction which could return the value of the result. This function returns `AccT`, which is an AST node. We could only extract AST structure from this data, not the value of the result. So there should be an "evaluator" of this AST structure.
2. How could we build the AST? Mainly with 2 kinds of functions:
    1. about **data**: `use` could wrap array of data into a struct called `ArrayT`
    2. about **expression**: `zipWith` is one of the expression wrappers, which could take nodes of data or expressions as its children.

Now it's time to run the code, and the whole procedure is just about iterating in the AST. There're mainly 2 passes:

##First pass:

1. This pass has been wrapped in a function called `parseAcc`. It takes 2 parameters, one is the AST root, with type `AccT`, and the other is an empty map.
2. In parseAcc, it'll check the type of the AST node, the parameter:
    1. `AccExpr` type: This one is an expression.
    2. `AccTerm` type: This one is closely related to the data type. 
