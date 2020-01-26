with Lambda; use Lambda;
with Ada.Text_IO; use Ada.Text_IO;


procedure Main is
   I : Instructions;
   Prompt : constant string := "> ";
begin
   -- If input = terminal and no arguments provided, print the header and prompt for input.
   Put_Line("Unbounded, a Lambda Calculus interpreter");
   Put_Line("");

   -- If -h or invalid arguments print this help message
   Put_Line("Usage: unbounded [-h] [-e 'expression']");
   Put_Line("");

   Put     ("Default operation is to evaluate expressions in an interactive REPL.  ");
   Put     ("End of input is system defined.  ");
   Put_Line("");
   Put_Line("");

   Put_Line("  -h                prints this help message");
   Put_Line("  -e 'expression'   evaluate the expression in non-interactive mode");
   Put_Line("");

   -- The Read|Evaulate|Print Loop
   loop
      -- If interactive mode then issue prompt
      Put(Prompt);
      -- Read a line
      -- if EOF then exit
      exit;

      -- parse the line
      -- reduce the line
      -- print the line

   end loop;

   -- if interactive mode then cleanup the screen
   Put_Line("");

end Main;
