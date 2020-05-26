-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Use a simple recursive parser for the simplest computer language.
-- It would be nice to build an ADT for Instructions and hide the Multiway tree implementation.
-- 
-- Source:
-- lambda         - [This file] definitions and helper functions
-- lambda_REPL    - REPL and command line parsers
-- lambda_parser  - parse tree generator
-- lambda_reducer - optimises and reduces lambda expressions
-- 

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

Package Lambda is
   subtype Statement is String;  -- A Lambda Calculus Statement, eg fn x.x
   Max_Statement_Length : constant Integer := 80;
   Empty_Statement : constant String :=
      Ada.Strings.Fixed.Head("", Max_Statement_Length,' ');

   subtype Name_Type is Character range 'a' .. 'z';
   subtype Synonym_Type is Character range 'A' .. 'Z';

   type Element_Type is (L_Variable, L_Synonym, L_Function, L_Expression, L_Definition, L_Comments);
   -- L_Variable    : Variable name
   -- L_Synonym     : Synonym name
   -- L_Function    : Function definition
   -- L_Expression  : Expression
   -- L_Definition  : Synonym definition

   type Element_Record ( Element : Element_Type := L_Expression ) is record
      Name : Character;
      is_Explicit : Boolean := true;
      case Element is
         when L_Comments => Comments : Statement(1..Max_Statement_Length);
         when others => null;
      end case;
   end record;

   package Instructions is new Ada.Containers.Multiway_Trees
     (Element_Type => Element_Record);
   use Instructions;

   -- Exceptions
   Syntax_Error : exception;
   Recursion_Overflow : exception;
   Internal_Error : exception;

   Package SU renames Ada.Strings.Unbounded;

   function format ( I: Instructions.Tree ) return String;
   function format ( I: Instructions.Tree; Curs : Instructions.Cursor ) return String;

   type Log_Type is (Log_Parse, Log_Reduce, Log_Format);
   procedure Log (S: String);
   procedure Log (T : Log_Type; S: String);

   procedure Add_Synonym( Source : Instructions.Cursor );
   procedure Remove_Synonym( S : Statement );
   procedure List_Synonyms;

   Trace : Boolean := FALSE;           -- Trace the REPL
   Trace_Parse  : Boolean := FALSE;    -- Trace Parser detail
   Trace_Reduce : Boolean := FALSE;    -- Trace Reducer detail
   Trace_Format : Boolean := FALSE;    -- Trace Formatter detail

private
   Synonyms : Instructions.Tree := Instructions.Empty_Tree;

   function format_Element ( I : Instructions.tree; Curs : Instructions.Cursor ) return SU.Unbounded_String;

end Lambda;

