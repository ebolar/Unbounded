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

   -- type Element is record
   --    Element : Element_Type;
   --    Name : Character;
   --    is_Explicit : Boolean := true;
   -- end record;

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

   Package SU renames Ada.Strings.Unbounded;

private
   Synonyms : Instructions.Tree := Empty_Tree;
   procedure Add_Synonym( Source : Instructions.Cursor );
   procedure Remove_Synonym( S : Statement );
   procedure List_Synonyms;

   function Get_Statement return Statement;

   function format_Element ( I : Instructions.tree; Curs : Instructions.Cursor ) return SU.Unbounded_String;
   function format ( I: Instructions.Tree ) return String;
   function format ( I: Instructions.Tree; Curs : Instructions.Cursor ) return String;

   type Log_Type is (Log_Parse, Log_Reduce, Log_Format);
   procedure Log (S: String);
   procedure Log (T : Log_Type; S: String);

   Trace : Boolean := FALSE;           -- Trace the REPL
   Trace_Parse  : Boolean := FALSE;    -- Parser detail
   Trace_Reduce : Boolean := FALSE;    -- Reducer detail
   Trace_Format : Boolean := FALSE;    -- Formatter detail

end Lambda;

