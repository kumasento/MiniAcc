module AST where

-- First thing of AST definition: NODE DEFINITION
--
-- There're 2 types of nodes: VAR and EXP
-- In the implementation of Accelerate, VAR is Acc;
-- EXP should be defined on your own


-- Acc should contain the abstract data structure of Array
-- and also the low level corresponding infos.
--
-- However, we could just use the first part of function.
--

data A_Exp  = A_Var
            | A_
