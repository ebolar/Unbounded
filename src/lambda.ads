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
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;

Package Lambda is
   subtype Statement is String;  -- A Lambda Calculus Statement, eg fn x.x
   Max_Statement_Length : constant Integer := 80;
   Empty_Statement : constant String
     := Ada.Strings.Fixed.Head("",Max_Statement_Length,'_');

   subtype Name_Type is Character range 'a' .. 'z';
   subtype Synonym_Type is Character range 'A' .. 'Z';
   -- subtype Space_Type is Character ('_', ' ');

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
   --
   -- Currently parse_Expression is an iterative parser.
   -- Other function definitions would be required to support a
   -- recursive descent parser.
   function parse_Commands( S: Statement ) return Boolean;
   function parse_Expression( S: Statement ) return Instructions;

   -- TBD Recursive tokeniser - split the expression into
   -- TBD * Strings of variables
   -- TBD * Functions
   -- TBD * Sub Expressions containing Functions and/or Variables and/or Synonyms
   -- TBD * Comments
   -- TBD * Synonym definitions
   -- TBD * Synonyms
   -- TBD
   -- TBD Then parse the tokens to create Instructions for each.
   -- TBD
   -- TBD Separators are '(' ')' '=' '\' '?' '&' '.'
   -- TBD
   --
   --   parseABC x Statement --> Instructions x RestOfStatement
   --
   -- function parse_Variables( S: Statement ) return Instructions;
   -- function parse_Function( S: Statement ) return Instructions;
   -- function parse_Application( S: Statement ) return Instructions;
   -- function parse_Synonym( S: Statement ) return Instructions;
   -- function parse_Comment( S: Statement ) return Instructions;

   function reduce( I: Instructions ) return Instructions;
   function format( I: Instructions ) return Statement;

   -- TBD: Overload Ada.TextIO?
   function Get_Statement return Statement;

   -- Exceptions
   Syntax_Error : exception;
   Recursion_Overflow : exception;
   Internal_Error : exception;

private

   -- Lambda_Expression is an unparsed Lambda Calculus expression.
   -- It is the root element type of all parsed Lambda Instructions.
   type Lambda_Expression is tagged record
      ID : String(1..5);
   end record;

   -- Lambda_Variable is an instance of a variable in a Lambda Calculus, eg x
   Type Lambda_Variable is new Lambda_Expression with record
      Name : Name_Type;
      -- isBound : Boolean := False;
      -- Bound_To : Instruction(L_Name) := null;
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
   --package Lambda_Instructions is new Ada.Containers.Vectors
   --  (Index_Type => Natural,
   --   Element_Type => Lambda_Expression);

   package Lambda_Instructions is new Ada.Containers.Multiway_Trees
     (Element_Type => Lambda_Expression);
   use Lambda_Instructions;

   type Instructions is new Lambda_Instructions.Tree with Null Record;

   Synonyms : Lambda_Instructions.Tree := Empty_Tree;

end Lambda;


