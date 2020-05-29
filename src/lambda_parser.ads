-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Use a simple recursive parser for the simplest computer language.
-- It would be nice to build an ADT for Instructions and hide the Multiway tree implementation.
-- 

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

with Lambda; use Lambda;

Package Lambda_Parser is
   -- Lamdba statement parser
   function parse_Statement( S: Statement ) return Instructions.Tree;

private
   Level : Natural;

   function Locate( Stmt : Statement; Posn : Natural; Expected : Character ) return Natural;
   function Next_Is( Stmt : Statement; Posn : Natural; Expected : Character ) return Boolean;

   procedure parse_expression( Stmt : Statement; Posn : in out Natural; Inst : in out Instructions.Tree; Curs : Instructions.Cursor );
   -- procedure parse_synonym( Stmt : Statement; Posn : in out Natural; Inst : in out Instructions.Tree; Curs : Instructions.Cursor );
   -- procedure parse_function( Stmt : Statement; Posn : in out Natural; Inst : in out Instructions.Tree; Curs : Instructions.Cursor );
   -- procedure parse_comments( Stmt : Statement; Posn : in out Natural; Inst : in out Instructions.Tree; Curs : Instructions.Cursor );

end Lambda_Parser;

