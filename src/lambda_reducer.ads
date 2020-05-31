-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- What is the strategy for reducing an expression?
-- - Strict evaluation is bottom up (C, Fortran, Java, Ada, etc)
-- - Lazy evaluation is top down - left to right (Haskel, Lambda Calculus)
--
-- Start with lazy evaluation. Come back and do strict evaluation as an enhancement.
--
-- In what order do we perform alpha substitution, beta reduction, eta reduction
-- - beta and eta reduction can be performed in any order - result is the same
-- - alpha substitution renames bound variables so that they do not overlap with free variables being substituted
--   (Barendregt Variable Convention).  Need to check this prior to performing a beta substitution.
--
-- Reduction is iterative.  What is the stopping condition?
-- - A Normal form cannot be further reduced.  Next form = last form, ie Alpha-Equivalence.
-- - Is beta normal if cannot be beta reduced
-- - Is eta normal if cannot be eta reduced
-- - can end up in an infinite loop
-- - there may be more than one normal form
--
-- Simplest stopping condition is if the expression is just Beta Normal.
-- More complex stopping condition is if the expression is both Beta Normal & Eta Normal.  Could this lead to loops?  Is this useful?
-- 
-- Musings: 
-- Reduction rules are the functions of a calculi.  For example, the reduction rules of arithmetic are the tables of addition and multiplication of numerals.  
-- If the reduction rules for the Lambda calculus were encoded in configuration then this interpreter could easily be extended with additional functions, 
-- enabling it to be used for more complex types of calculus. (eg. could you turn this into a Lisp interpreter?)
--

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

-- package Instructions is new Ada.Containers.Multiway_Trees
--   (Element_Type => Element_Record);
-- use Instructions;

with Lambda; use Lambda;

Package Lambda_Reducer is

   function reduce( I: Instructions.Tree ) return Instructions.Tree;

private

   -- Alpha-renaming to preserve the meaning of a function
   -- function rename( I: Instructions.Tree ) return Instructions.Tree;

   -- Beta-reduction, Eta-reduction.  
   -- Variable substitution, eg (?x.x)[y:=y] = ?x.(x[y:=y]) = ?x.x
   -- function substitution( I: Instructions.Tree ) return Instructions.Tree;
   --
   -- Test for alpha-equivalence.  Can we use this to detect when we are in a recursion loop?
   -- 
   --

   -- Expression Optimiser 
   -- - ((X)) => (X)
   -- - X() => X
   procedure Optimise( I: in out Instructions.Tree ; Curs : Instructions.Cursor );

end Lambda_Reducer;

