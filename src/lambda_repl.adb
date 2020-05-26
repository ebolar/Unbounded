-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Source:
-- lambda         - definitions and helper functions
-- lambda_REPL    - [This file] REPL and command line parsers
-- lambda_parser  - parse tree generator
-- lambda_reducer - optimises and reduces lambda expressions
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;

with lambda; use lambda;
with lambda_parser; use lambda_parser;
with lambda_reducer; use lambda_reducer;

Package body Lambda_REPL is

   -- The Read|Evaulate|Print Loop
   procedure REPL is
      I : Instructions.Tree := Instructions.Empty_Tree;
      Prompt : constant string := "Unbounded> ";

      S : Statement (1..Max_Statement_Length);
      L : Natural;

   begin
      -- TBD: Load the environment context
      -- (environment settings, eg trace on/off)
      -- (library of Synonyms)

      L := 0;
      loop
         L := L+1;

         begin
            --Put(Integer'Image(L) & " " & Prompt);
            Put(Prompt);

            -- Read a line
            S := Get_Statement;
            New_Line;

            -- parse the line
            if not parse_Commands(S) then
               I := parse_Statement(S);

               -- reduce the line
               I := reduce(I);

               -- print the line
               Put_Line("-> " & format(I));
            end if;

         exception
            when Ada.IO_Exceptions.END_ERROR =>
               New_Line;
               exit;
            when E: others =>
               Put_Line(Exception_Name(E) & ": "
                        & Exception_Message(E) & " in "
                        & Ada.Strings.Fixed.Trim(Source => S, Side => Both)
                        );
         end;

      end loop;
      Put_Line("Bye");

      -- TBD: Save the environment context if required
   end;

   -- Command line Lambda expression parser
   procedure Evaluate ( S : in Statement ) is
   begin
      null;
   end;

   -- Parse commands from the REPL
   --
   -- Commands are:
   -- : "EXIT" | "QUIT"    : Exit
   -- : "HELP"             : Print the help/usage message
   -- : "LS"               : List saved expressions
   -- : "RM <symbol>"      : Delete an expression
   -- : "TRACE"            : Display tracing level
   -- : "TRACE <feature> " : Set verbose tracing for [ON|OFF|PARSE|REDUCE|FORMAT] functions
   function parse_Commands( S: Statement ) return Boolean is
      X : SU.Unbounded_String;
      Token : String := Empty_Statement;
      From : Natural := 1;

      -- REPL command tokeniser
      --  Command => the statement to tokenise
      --  From    => current position in the Command.  Returns = when end of Command reached
      --  Token   => next token extracted from the Command
      --
      procedure Next_Token( Command : Statement; From : in out Natural; Token : out Statement ) is
         Cmd_Set : Character_Set := To_Set( Sequence => "ABCDEFGHIJKLMNOPQRSTUVWXYZ-abcdefghijklmnopqrstuvwxyz");
         First : Positive;
         Last : Natural;
      begin
         Find_Token(Command, Cmd_Set, From, Inside, First, Last);

         if Last = 0
         then
            Token := Empty_Statement;
            From := 0;
         else
            Token := Ada.Strings.Fixed.Head( Command(First..Last), Max_Statement_Length, ' ');
            if Last < Command'Last
            then
               From := Last + 1;
            else
               From := 0;
            end if;
         end if;
      end;

   begin
      From := 1;
      next_token(S, From, Token);
      X := SU.To_Unbounded_String(To_Upper(trim(Token, Both)));

      if X = "EXIT" OR X = "QUIT"
      then
         Log("Exit");
         raise Ada.IO_Exceptions.END_ERROR;

      elsif X = "LS"
      then
         Log("List saved expressions");
         List_Synonyms;
         return True;

      elsif X = "RM"
      then
         Log("Remove synonym");
	 loop
            next_token(S, From, Token);
	    exit when From = 0;

	    Remove_Synonym(Token);
	 end loop;

         return True;

      -- This is a little ugly
      elsif X = "TRACE"
      then
         loop
            next_token(S, From, Token);
            exit when From = 0;

            X := SU.To_Unbounded_String(To_Upper(trim(Token, Both)));

            if X = "PARSE" then
               Trace_Parse := not Trace_Parse;
            elsif X = "REDUCE" then
               Trace_Reduce := not Trace_Reduce;
            elsif X = "FORMAT" then
               Trace_Format := not Trace_Format;
            elsif X = "ON" then
               Trace := TRUE;
            elsif X = "OFF" then
               Trace := FALSE;
            else
               raise Syntax_Error with "Invalid parameter: " & To_String(X);
            end if; 
         end loop;

         if Trace
         then
            Put(". Trace on");
            if Trace_Parse then
               Put(" - PARSE");
            end if;
            if Trace_Reduce then
               Put(" - REDUCE"); 
            end if;
            if Trace_Format then
               Put(" - FORMAT");
            end if;
            new_line;
         else
            Put_Line(". Trace off");
         end if;

         return True;

      elsif X = "HELP"
      then
         Put_Line("EXIT | QUIT     : Exit Unbounded");
         Put_Line("HELP            : Display this message");
         Put_Line("LS              : List all saved expressions");
         Put_Line("RM <symbol>     : Delete an expression");
         Put_Line("TRACE           : Display the tracing level");
         Put_Line("TRACE <feature> : Set verbose tracing [ON|OFF|PARSE|REDUCE|FORMAT]");
         return True;
      end if;

      return False;
   end parse_Commands;


   -- ==========================================================================================
   -- Private functions
   -- ==========================================================================================
   --
   -- Input a Lambda statement
   function Get_Statement return Statement is
      Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;
   begin
      Buffer := Ada.Text_IO.Unbounded_IO.Get_Line;

      return Ada.Strings.Fixed.Head(Source => SU.To_String(Buffer),
                                    Count => Max_Statement_Length,
                                    Pad => ' ');
   end Get_Statement;


end Lambda_REPL;

