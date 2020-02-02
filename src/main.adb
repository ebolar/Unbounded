-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- main.adb contains the user interface, including
-- * Commandline argument interpreter
-- * REPL
-- * Unit testing code for the Lambda package
--
-- TBD
-- *
--
-- Other ideas
-- * Pretty print the banner - How do you format a Lambda symbol

with Lambda; use Lambda;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;

procedure Main is

begin
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

   -- if args contain -u then run the unit tests
   -- I := parse("I = fn x.x");
   -- I := parse("");

   -- if args contain -q then set quiet_mode
   -- elseif args contain -v then set trace_mode;

   -- if args contain -e then
   --    evaluate expression
   -- else
   --    REPL
   -- end if;

   -- The Read|Evaulate|Print Loop
   REPL;
   

end Main;
