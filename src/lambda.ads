-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Use a simple parser for now.  Full lexical parser is overkill.
--
-- TBD
-- * How do I add documentation to this?

-- with Ada.Strings.Bounded;
with Ada.Strings.UnBounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

Package Lambda is
   subtype Statement is String;  -- A Lambda Calculus Statement, eg fn x.x
   subtype Statement_Length is Natural;
   Max_Statement_Length : constant Integer := 50;

   type Instructions is tagged private;

   -- TBA: a library of named functions
   -- eg I=Fn x.x

   function parse( S: Statement ) return Instructions;
   function reduce( I: Instructions ) return Instructions;
   function format( I: Instructions ) return Statement;

   procedure Put_Statement( S: in Statement);
   function Get_Statement return Statement;

   -- Exceptions
   Syntax_Error : exception;
   Recursion_Overflow_Error : exception;
   Internal_Error : exception;

private
   subtype Name_Type is Character range 'a' .. 'z';
   subtype Synonym_Type is Character range 'A' .. 'Z';

   -- Instructions are a complex combination of Functions and Variables
   -- Is Lambda_Application the root type in the heirarchy?

   -- Lambda_Expression is an unparsed Lambda Calculus expression.
   -- It is the root element type of all parsed Lambda instructions.
   --
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

   type Lambda_Application is new Lambda_Expression with record
      Value : Statement(1..Max_Statement_Length);
      -- Expressions
   end record;

   -- Hm... Would like to pass LE'Class however Vectors need a definite subtype
   -- Means that I need to LE(xxx) when I add elements.
   package Lambda_Instructions is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Lambda_Expression);

   type Instructions is new Lambda_Instructions.Vector with Null Record;

   -- how do I restrict Synonyms to only Synonyms?
   type Synonyms is new Lambda_Instructions.Vector with Null Record;

end Lambda;


