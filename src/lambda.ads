-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Use a simple recursive parser for the simplest computer language.
-- It would be nice to build an ADT for Instructions and hide the Multiway tree implementation.

with Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

Package Lambda is
   subtype Statement is String;  -- A Lambda Calculus Statement, eg fn x.x
   Max_Statement_Length : constant Integer := 80;
   Empty_Statement : constant String :=
      Ada.Strings.Fixed.Head("", Max_Statement_Length,'_');

   subtype Name_Type is Character range 'a' .. 'z';
   subtype Synonym_Type is Character range 'A' .. 'Z';

   type Element_Type is (L_Variable, L_Synonym, L_Function, L_Expression, L_Definition);
   -- L_Variable    : Variable name
   -- L_Synonym     : Synonym name
   -- L_Function    : Function definition
   -- L_Expression  : Expression
   -- L_Definition  : Synonym definition

   type Element is record
      Element : Element_Type;
      Name : Character;
      is_Explicit : Boolean := true;
   end record;

   package Instructions is new Ada.Containers.Multiway_Trees
     (Element_Type => Element);
   use Instructions;

   -- Processing functions
   procedure REPL;

   -- REPL command parser
   function parse_Commands( S: Statement ) return Boolean;
   -- Lamdba statement parser
   function parse_Statement( S: Statement ) return Instructions.Tree;

   -- Alpha-renaming to preserve the meaning of a function
   -- function rename( I: Instructions.Tree ) return Instructions.Tree;

   -- Beta-reduction, Eta-reduction.  When do you use these?  What is the strategy?
   function reduce( I: Instructions.Tree ) return Instructions.Tree;
   -- Variable substitution, eg (?x.x)[y:=y] = ?x.(x[y:=y]) = ?x.x
   -- function substitution( I: Instructions.Tree ) return Instructions.Tree;

   -- Exceptions
   Syntax_Error : exception;
   Recursion_Overflow : exception;
   Internal_Error : exception;

private
   Synonyms : Instructions.Tree := Empty_Tree;
   procedure Add_Synonym( Source : Instructions.Cursor );
   procedure Remove_Synonym( S : Statement );
   procedure List_Synonyms;

   Package SU renames Ada.Strings.Unbounded;

   function Get_Statement return Statement;

   function format_Element ( I : Instructions.tree; Curs : Instructions.Cursor ) return SU.Unbounded_String;
   function format ( I: Instructions.Tree ) return String;
   function format ( I: Instructions.Tree; Curs : Instructions.Cursor ) return String;

   procedure Log (S: String);

   Trace : Boolean := True;

end Lambda;

