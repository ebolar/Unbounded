-- Simple Lambda Calculus interpreter
-- ----------------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- The syntax roughly corresponds to what could be written in a text on the
-- Lambda Calculus.  
-- * Spaces and underscores are ignored
-- * Single letter variable names 'a'..'z'
-- * Single letter Synonyms 'A'..'Z'.
-- * Expressions can be nested with parentheses '('|')'
-- * Functions start with '?', '/' or '&', eg ?<name>.<expression>
-- * # The rest of the line is a comment
-- * any other character generates a SYNTAX_ERROR
-- 
-- Conventions 
-- * Expressions are associative from left to right.
--   ABC == ((AB)C)
-- * ?abc.<expression> is eqivalent to ?a.(?b.(?c.<expression>)))
-- * Longest valid expression wins.  Function expresions go to the end of scope.
--   ?abc.ABC == ?abc.((AB)C)
--   ?abc.(AB)C == ?abc.((AB)C)
--   (?abc.AB)C == (?abc.(AB))(C) ie C applied to (?abc.AB)
--
-- The REPL also allows
-- * <synonym>=<expression>  - declare or replace a synonym
-- * <synonym>=              - delete a synonym
-- * LS                      - list all synonyms
-- * QUIT|EXIT|<END_OF_FILE> - Exit
-- * TRACE-ON|TRACE-OFF      - Turn on/off verbose tracing
--
-- REPL commands are not case sensitive
-- 
-- Unit testing is performed via the make script.  
--
-- TBD
-- * Save/Restore synonyms to a file
--

with Lambda; use Lambda;
with Ada.Text_IO; use Ada.Text_IO;

procedure unbounded is

begin
   Put_Line("       __");
   Put_Line("       \ \");
   Put_Line("  ___   \ \");
   Put_Line("  ___    . \");
   Put_Line("  ___   / ^ \");
   Put_Line("       /_/ \_\");
   New_Line;
      
   -- If input = terminal and no arguments provided, print the header and prompt for input.
   Put_Line("Unbounded, a Lambda Calculus interpreter");
   Put_Line("");

   -- If args contain -h or invalid arguments then print the help message
   Put_Line("Usage: unbounded [-h] [-q | -v] [-e 'expression']");
   Put_Line("");

   Put     ("Default operation is to evaluate expressions in an interactive REPL.  ");
   Put     ("End of input is system defined.  ");
   Put_Line("");
   Put_Line("");

   Put_Line("  -h                prints this help message.");
   Put_Line("  -q                run quietly.");
   Put_Line("  -v                verbose tracing.");
   Put_Line("  -e 'expression'   evaluate the expression in non-interactive mode.");
   Put_Line("");

   -- if args contain -q then set quiet_mode
   -- elseif args contain -v then set trace_mode;

   -- if args contain -e then
   --    evaluate expression
   -- else
   --    The Read|Evaulate|Print Loop
   REPL;
   -- end if;

end unbounded;
