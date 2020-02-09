-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Use a simple parser for now.  Full lexical parser is overkill.
--
-- TBD
-- * Get_Line() return Statement (1..Max_Statement_Length)
-- * Make Statement be unbounded!!!

-- with Ada.Strings.Bounded;
-- with Ada.Strings.UnBounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

Package Lambda is
   subtype Statement is String;  -- A Lambda Calculus Statement, eg fn x.x
   Max_Statement_Length : constant Integer := 80;
   Empty_Statement : constant String
     := Ada.Strings.Fixed.Head("",Max_Statement_Length,'_');

   type Instructions is tagged private;

   -- Processing functions
   procedure REPL;
   procedure Evaluate( S: in Statement );

   -- A simple Lambda Calculus Parser
   -- -------------------------------
   -- Separate functions for parsing:
   -- * Commands entered into the REPL
   -- * Function variables
   -- * Full Lambda Calculus expresions
   function parse_Commands( S: Statement ) return Boolean;
   function parse_Variables( S: Statement ) return Instructions;
   function parse_Expression( S: Statement ) return Instructions;
   function reduce( I: Instructions ) return Instructions;
   function format( I: Instructions ) return Statement;

   -- TBD: Overload Ada.TextIO?
   function Get_Statement return Statement;

   -- Exceptions
   Syntax_Error : exception;
   Recursion_Overflow : exception;
   Internal_Error : exception;

private


   subtype Name_Type is Character range 'a' .. 'z';
   subtype Synonym_Type is Character range 'A' .. 'Z';
   -- subtype Space_Type is Character ('_', ' ');

   -- Lambda_Expression is an unparsed Lambda Calculus expression.
   -- It is the root element type of all parsed Lambda instructions.
   type Lambda_Expression is tagged record
      ID : String(1..5);
   end record;

   -- Lambda_Variable is an instance of a variable in a Lambda Calculus, eg x
   Type Lambda_Variable is new Lambda_Expression with record
      Name : Name_Type;
      isBound : Boolean := False;
   end record;

   -- Lambda_Function is a Lambda Calculus function, eg fn x.x
   -- A Lambda_Function may be named, eg I=fn x.x
   type Lambda_Function is new Lambda_Expression with record
      Name : Synonym_Type;
      Value : Statement(1..Max_Statement_Length);
      -- Variables
      -- Expressions
   end record;

   -- Lambda_Application is a collection of Lambda_Expressions,
   -- for example I I, that are evaluated as a single expression.
   type Lambda_Application is new Lambda_Expression with record
      Value : Statement(1..Max_Statement_Length);
      -- Expressions
   end record;

   -- Lambda_Instructions are parsed Lambda_Expressions.
   -- Lambda_Instructions can be reduced or displayed.
   --
   -- Notes: I would like to use the class-wide Lambda_Elements'Class element type definition
   -- however Vectors need a definite subtype.
   -- * This means that I need to use Lambda_Elements(xxx) when I add elements.
   package Lambda_Instructions is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Lambda_Expression);

   type Instructions is new Lambda_Instructions.Vector with Null Record;

   -- how do I restrict Synonyms to only Synonyms?
   -- type Synonyms is new Lambda_Instructions.Vector with Null Record;

end Lambda;


