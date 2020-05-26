-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Use a simple recursive parser for the simplest computer language.
-- It would be nice to build an ADT for Instructions and hide the Multiway tree implementation.
-- 
-- TBD:
-- Source is getting too big.  Split into two packages: Lambda_REPL and Lambda_CORE

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

with Lambda; use Lambda;

Package Lambda_Reducer is

   function reduce( I: Instructions.Tree ) return Instructions.Tree;

   -- Expression Optimiser 
   -- - ((X)) => (X)
   -- - X() => X
   --
   -- Alpha-renaming to preserve the meaning of a function
   -- function rename( I: Instructions.Tree ) return Instructions.Tree;

   -- Beta-reduction, Eta-reduction.  When do you use these?  What is the strategy?
   -- Variable substitution, eg (?x.x)[y:=y] = ?x.(x[y:=y]) = ?x.x
   -- function substitution( I: Instructions.Tree ) return Instructions.Tree;

end Lambda_Reducer;

