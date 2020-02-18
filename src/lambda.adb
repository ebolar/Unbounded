-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;

Package body Lambda is

   package SF renames Ada.Strings.Fixed;

   -- The Read|Evaulate|Print Loop
   procedure REPL is
      I : Instructions; -- := Empty_Tree;
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
            Put(Integer'Image(L) & " " & Prompt);

            -- Read a line
            S := Get_Statement;
	    New_Line;
            Put_Line(". [" & S & "]");

            -- parse the line
            if not parse_Commands(S) then
               I := parse_Expression(S);

               -- reduce the line
               I := reduce(I);

               -- print the line - not yet
               -- Put_Line("... " & format(I));
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

   -- A simple Lambda parser
   -- ----------------------
   --
   -- Ignore spaces.
   --
   -- Separate processing required for parsing:
   -- * Commands for the REPL
   -- * Lambda expresions
   -- * Function variables
   function parse_Commands( S: Statement ) return Boolean is
   begin
      -- First check for commands at the start of a line
      -- : "TRACE-ON" : Verbose tracing
      -- : "TRACE-OFF" : Normal
      -- : "EXIT" | "QUIT" : Exit
      -- : "LS" : List saved expressions
      if SF.Index(Source => To_Upper(S), Pattern => "EXIT") = 1 or
         SF.Index(Source => To_Upper(S), Pattern => "QUIT") = 1
      then
         Put_Line(". Exit");
         raise Ada.IO_Exceptions.END_ERROR;

      elsif SF.Index(Source => To_Upper(S), Pattern => "LS") = 1
      then
            Put_Line(". List saved expressions");
         return True;

      elsif SF.Index(Source => To_Upper(S), Pattern => "TRACE-ON") = 1
      then
            Put_Line(". Trace on");
         return True;

      elsif SF.Index(Source => To_Upper(S), Pattern => "TRACE-OFF") = 1
      then
            Put_Line(". Trace off");
         return True;
      end if;

      return False;
   end parse_Commands;

   function parse_Expression( S: Statement ) return Instructions is
      Default : Instructions;
      Temp : Instructions;

      E : Character;

      Level : Natural := 0;	-- One day I want WONDER:NATURAL;
      F_Level : Natural := 0;

      type Parse_Mode is (L_Null, L_Expression, L_Function);
      Parsing : Parse_Mode := L_Null;

      procedure indent( I : Natural ) is
         L : Natural := 0;
      begin
         while L < I loop
            put(" ");
            L := L+1;
         end loop;
      end;

   begin
      -- Name_Type : [a-z] : Variable
      -- Synonym_Type : [A-Z] : Synonym - name of a function
      -- : [?|&] : Start of a Function definition.  TBD parse "fn "
      -- : [.] : separates Function Variables from Expression
      -- : [(] : starts a new Expression
      -- : [)] : ends an Expression
      -- : [=] : assigns a Function to a Synonym
      -- : [_| ] : Spaces.  Ignore these.
      --

      --
      -- Iterative parser
      -- (OK a recursive parser would be simpler)
      for C in S'Range loop
         E := S(C);
         case E is
            when Name_Type =>
               Put ("... " );
               Indent (Level);
               Put_Line ("Variable: " & E);
            when Synonym_Type =>
               Put ("... " );
               Indent (Level);
               Put_Line("Synonym: " & E);
               -- if next character is '=' then
               --   this is a declaration.  Store in the environment.
               -- else
               --   lookup the Synonym
               --   if Synonym is found then insert the reference
               --   else Syntax error
            when '?' | '&' | '\' =>
               Put ("... " );
               Indent (Level);
               Put_Line("Function - Variables");
               Level := Level + 1;
               F_Level := F_Level + 1;

               -- * look for the .
               -- * Parse the string returning I into Function.Variables
               -- * Look for the end of the function expression.  Rule is
               -- to take the longest possible expression, ie either EOL
               -- or closing ).
               -- * Parse the string returning I into Function.Expression
               -- * Must contain a .  If not then Syntax error
               -- ??? Would up/down functions be useful?
            when '.' =>
               begin
                  Put ("... " );
                  Indent (Level - 1);
                  Put_Line("Function - Expression");
               exception
                  when Constraint_Error =>
                     raise Syntax_Error with "Unexpected '.' - no function declared";
               end;

            when '(' =>
               Put ("... " );
               Indent (Level);
               Put_Line("(");
               Level := Level + 1;
               Parsing := L_Expression;

	       -- parse the rest of the line as level +1.
	       -- Recursive overflow exception
            when ')' =>
               begin
                  Level := Level - 1;
                  Put ("... " );
                  Indent (Level);
                  Put_Line(")");
                  -- parse the rest of the line as level -1
               exception
                  when Constraint_Error =>
                     raise Syntax_Error with "Unmatched ')'";
               end;

            when '=' =>
               Put ("... " );
               Indent (Level);
               Put_Line("= ");
            when '_' | ' ' => null;
	    when '#' =>
               Put ("... " );
               Indent (Level);
	       Put_Line("#");
               exit;  -- Ignore the rest of the line
            when others => raise Syntax_Error with "Invalid character";
         end case;

      end loop;

      -- Check for missing ')'.
      -- Needs separate Level counters for () and functions.
      if Level - F_Level > 0 then

         raise Syntax_Error with "Missing ')'";
      end if;

      return Default;
   end parse_Expression;

   -- Beta reduction algorithm
   function reduce( I: Instructions ) return Instructions is
      Default : Instructions;
   begin
      if I.Is_Empty then
         null;
      end if;

      return Default;
   end reduce;

   function format( I: Instructions ) return Statement is
      Default : Statement := Empty_Statement;
   begin
      if I.Is_Empty then
         null;
      end if;

      return Default;
   end format;

   function Get_Statement return Statement is

      package SU renames Ada.Strings.Unbounded;

      Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;

   begin
      Buffer := Ada.Text_IO.Unbounded_IO.Get_Line;

      return Ada.Strings.Fixed.Head(Source => SU.To_String(Buffer),
                                    Count => Max_Statement_Length,
                                    Pad => ' ');
   end Get_Statement;

end Lambda;

