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

Package Lambda_Parser is
   -- Lamdba statement parser
   function parse_Statement( S: Statement ) return Instructions.Tree;

end Lambda_Parser;

