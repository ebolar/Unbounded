-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Use a simple recursive parser for the simplest computer language.
-- It would be nice to build an ADT for Instructions and hide the Multiway tree implementation.
-- 
-- Source:
-- lambda         - definitions and helper functions
-- lambda_REPL    - [This file] REPL and command line parsers
-- lambda_parser  - parse tree generator
-- lambda_reducer - optimises and reduces lambda expressions
-- 

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

with Lambda; use Lambda;
with lambda_parser; use lambda_parser;
with lambda_reducer; use lambda_reducer;

Package Lambda_REPL is
   -- Processing functions
   procedure REPL;
   procedure Evaluate ( S : in Statement );

   -- REPL command parser
   function parse_Commands( S: Statement ) return Boolean;

private

   function Get_Statement return Statement;

end Lambda_REPL;

