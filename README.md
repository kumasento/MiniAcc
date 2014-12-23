
#Introduction to MiniAcc

##Introduction

**MiniAcc** is a subset of a well-known Haskell parallel programming DSL -- Accelerate. As Accelerate only has NVIDIA CUDA as its backend, MiniAcc is focusing on providing OpenCL as it's backend. 
MiniAcc's front-end code is following an AST-style, which means the Haskell code you've written will be evaluated as an AST node of MiniAcc. Then this node will be passed to a parsing procedure. This parsing procedure will return the generated OpenCL code, including `variable declaration`, `lambda function declaration` and `calculation procedures`.
This report will be extended into 3 parts:

1. MiniAcc's AST-style code
2. MiniAcc's parser and code generator
3. MiniAcc's backend code and performance

