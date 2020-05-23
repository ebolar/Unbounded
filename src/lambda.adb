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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;

-- TBD:
-- Build an ADT for Instructions that makes the logic readable.
--
Package body Lambda is

   -- The Read|Evaulate|Print Loop
   procedure REPL is
      I : Instructions.Tree := Empty_Tree;
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
   -- TBD:
   -- - Needs a tokeniser to allow for arguments to be passed to commands
   --   Some selection of:
   --    String x Natural x Next_Token() -> String | Constraint_Error
   --    String x Natural x No_More_Tokens() -> Boolean
   --   ...
   --    String x Token(Natural) -> String | Constraint_Error
   --    String x Token_Count() -> Natural
   --   ...
   --    String x Tokenise() -> String(0..n);
   --
   function parse_Commands( S: Statement ) return Boolean is
      X : SU.Unbounded_String;
      Token : String := Empty_Statement;
      From : Natural := 1;

      -- REPL command tokeniser
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
      -- Check for commands
      -- : "EXIT" | "QUIT"    : Exit
      -- : "HELP"             : Print the help/usage message
      -- : "LS"               : List saved expressions
      -- : "RM <symbol>"      : Delete an expression
      -- : "TRACE"            : Display tracing level
      -- : "TRACE <feature> " : Set verbose tracing for [ON|OFF|PARSE|REDUCE|FORMAT] functions

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

   -- ----------------------------------------------
   -- Recursive descent parser for Lambda statements
   -- ----------------------------------------------
   -- The simplest parser for the simplest computer language.
   --
   -- Name_Type    : [a-z]   : Variable
   -- Synonym_Type : [A-Z]   : Synonym - name of a function
   --              : [?|&|\] : Start of a Function definition.
   --              : [.]     : separates Function Variables from Expression
   --              : [(]     : starts a new Expression
   --              : [)]     : ends an Expression
   --              : [=]     : assigns a Function to a Synonym
   --              : [_| ]   : Spaces.  Ignore these for now.
   --                        : - May be useful down the track to collapse these into a single spacer element.
   --                        :   This would enable the definition of multicharacter Synonyms, eg KI.
   --
   function parse_Statement( S: Statement ) return Instructions.Tree is
      Default : Instructions.Tree;
      Index : Instructions.Cursor;

      X : Statement := trim(S, Left);
      Level : Natural := 0;

      -- E : Character;
      C : Natural := 1;

      function indent( I : Natural ) return String is
         -- should be more than long enough
         M : String := Ada.Strings.Fixed.Head("", Max_Statement_Length/2, ' ');
      begin
         return M(1..I);
      end;

      function Tree_Level(Curs : Instructions.Cursor) return Natural is
      begin
         return Natural(Depth(Curs)) - 1;
      end;

      -- TBD:
      -- - Break these out into multiple functions.  Would a separate package be useful?
      -- - Cleanup the code - using Next_Is and Locate helper functions.
      -- - Automatically create containers - dont need to check for explicit '('.
      -- - Reducer can optimise ((X)) to (X)!
      --
      -- parse_expression()
      procedure parse_expression( Stmt : Statement; Posn : in out Natural; Inst : in out Instructions.Tree; Curs : Instructions.Cursor ) is
         E : Character;
         Node : Element_Record;
         Index : Instructions.Cursor;

         function Next_Is(Stmt : Statement; Posn : Natural; Expected : Character) return boolean is
            Loc : Natural;
            E : Character;
         begin
            Loc := Posn + 1;
            while Loc <= Stmt'Length loop
               E := Stmt(Loc);
               if E = Expected then
                       return true;
               else
                  case E is
                     when '_' | ' ' => Loc := Loc + 1;
                     when others => return false;
                  end case;
               end if;
            end loop;
            return false;
         end;

         function Locate(Stmt : Statement; Posn : Natural; Expected : Character) return natural is
            Loc : Natural := Posn;
            E : Character;
         begin
            while Loc <= Stmt'Length loop
               E := Stmt(Loc);
               if E = Expected then
                       return Loc;
               else
                       Loc := Loc + 1;
               end if;
            end loop;
            return Loc;
         end;

      begin
         while Posn <= Stmt'Length loop
            E := Stmt(Posn);

            case E is
               when Name_Type =>
                  if Instructions.Is_Empty (Inst)
                  then
                     --   Insert(L_Expression)
                     Node := (Element => L_Expression, Name => '_', Is_Explicit => false);
                     Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);

                     --   move Cursor and reprocess
                     parse_expression(Stmt, Posn, Inst, First_Child(Parent => Curs));

                  else
                     Log(Log_Parse, Indent(Level) & "Variable: " & E);

                     -- Insert(L_Variable)
                     Node := (Element => L_Variable, Name => E, is_Explicit => true);
                     Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);
                  end if;

               -- parse_synonym()
               when Synonym_Type =>
                  -- if empty tree and next character is '=' then
                  if Instructions.Is_Empty(Inst) and next_is( Stmt, Posn, '=') then
                     --   Insert (L_Definition (L_Symbol, L_Expression ...)
                     Log(Log_Parse, Indent (Level) & "Synonym: " & E);
                     Log(Log_Parse, Indent (Level) & "= ");
                     Node := (Element => L_Definition, Name => E, is_Explicit => false);
                     Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);

                     Posn := Locate(Stmt, Posn, '=');
                     Index := Last_Child(Parent => Curs);

                     if next_is( Stmt, Posn, '(')
                     then
                        Posn := Locate(Stmt, Posn, '(');
                        parse_expression(Stmt, Posn, Inst, Index);
                     else
                        Node := (Element => L_Expression, Name => '_', Is_Explicit => false);
                        Append_Child(Container => Inst,
                                     Parent    => Index,
                                     New_Item  => Node);

                        Posn := Posn + 1;
                        parse_expression(Stmt, Posn, Inst, Last_Child(Parent => Index));
                     end if;

                     Add_Synonym( Source=> Index );

                  else
                     if Instructions.Is_Empty (Inst)
                     then
                        --   Insert(L_Expression)
                        Node := (Element => L_Expression, Name => '_', is_Explicit => false);
                        Append_Child(Container => Inst,
                                     Parent    => Curs,
                                     New_Item  => Node);

                        --   move Cursor and reprocess
                        parse_expression(Stmt, Posn, Inst, Last_Child(Parent => Curs));

                     else
                        Log(Log_Parse, Indent (Level) & "Synonym: " & E);

                        --   Insert (L_Synonym)
                        Node := (Element => L_Synonym, Name => E, is_Explicit => true);
                        Append_Child(Container => Inst,
                                     Parent    => Curs,
                                     New_Item  => Node);
                     end if;
                  end if;

               -- parse_function()
               when '?' | '&' | '\' =>

                  if Instructions.Is_Empty (Inst)
                  then
                     --   Insert(L_Expression)
                     Node := (Element => L_Expression, Name => '_', is_Explicit => false);
                     Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);

                     --   move Cursor and reprocess
                     parse_expression(Stmt, Posn, Inst, Last_Child(Parent => Curs));

                  else
                     Node := Instructions.Element(Curs);

                     -- Functions are an implicitly defined container
                     Node := (Element => L_Function, Name => E, is_Explicit => false);
                     Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);

                     Index := Last_Child(Parent => Curs);

                     Log(Log_Parse, Indent (Level) & "Function - Variables");
                     Level := Level + 1;

                     Posn := Posn + 1;
                     while Posn <= Stmt'Length loop
                        E := Stmt(Posn);
                        Posn := Posn + 1;

                        exit when E = '.';

                        case E is
                           when Name_Type =>
                              Log(Log_Parse, Indent(Level) & "Variable: " & E);

                              -- Insert(L_Variable)
                              Node := (Element => L_Variable, Name => E, is_Explicit => true);
                              Append_Child(Container => Inst,
                                           Parent    => Index,
                                           New_Item  => Node);
                           when others => raise Syntax_Error with "Malformed function declaration";
                        end case;
                     end loop;
                     Log(Log_Parse, Indent (Level - 1) & "Function - Expression");

                     --   if next character is not '('
                     --   (needs a next routine that skips spaces)
                     begin
                        if Stmt(Posn) /= '('
                        then
                           --   Insert(implied L_Expression)
                           Node := (Element => L_Expression, Name => '_', is_Explicit => false);
                           Append_Child(Container => Inst,
                                        Parent    => Index,
                                        New_Item  => Node);

                           Index := Last_Child(Parent => Index);

                        end if;
                     exception
                        when Constraint_Error =>
                           raise Program_Error with "Buffer overflow";
                     end;

                     Node := Instructions.Element(Index);

                     --   Process the expression
                     parse_expression(Stmt, Posn, Inst, Index);
                  end if;

               when '(' =>
                  Log(Log_Parse, Indent (Level) & "(");

                  if not Instructions.Is_Root(Curs)
                  then
                     Node := Instructions.Element(Curs);
                  end if;

                  Level := Level + 1;

                  --   Insert(L_Expression)
                  Node := (Element => L_Expression, Name => '_', is_Explicit => true);
                  Append_Child(Container => Inst,
                               Parent    => Curs,
                               New_Item  => Node);

                  --   move Cursor and parse the sub-expression
                  Posn := Posn + 1;
                  parse_expression(Stmt, Posn, Inst, Last_Child(Parent => Curs));

                  if Posn > Stmt'Length
                  then
                     raise Syntax_Error with "Missing ')'";
                  end if;

               when ')' =>
                  begin
                     Node := Instructions.Element(Curs);
                     Level := Level - 1;

                     if Node.is_Explicit then
                        Log(Log_Parse, Indent (Level) & ")");
                     else
                        -- If we are dealing with an implicitly defined container
                        -- and we encounter a ')',
                        -- it belongs to the enclosing expression and not this one!
                        Log(Log_Parse, Indent (Level) & ".");
                        Posn := Posn - 1;
                     end if;

                     return;
                  exception
                     when Constraint_Error =>
                        raise Syntax_Error with "Unmatched ')'";
                  end;

               -- parse_comments()
               when '#' =>
                  Log(Log_Parse, Indent (Level) & "#");

                  --   Insert(L_Comments)
                  Node := (Element => L_Comments, Name => '#', is_Explicit => true, Comments => Empty_Statement);
                  Node.Comments := Ada.Strings.Fixed.Head(Stmt(Posn+1..Stmt'Last), Max_Statement_Length, ' ');

                  declare
                     First : Element_Record;
                     Location : Instructions.Cursor := Root(Inst);
                  begin
                     -- if first child is a symbol definition then append to the definition
                     if not Instructions.Is_Empty(Inst)
                     then
                        First := First_Child_Element(Location);
                        if First.Element = L_Definition
                        then
                           Location := First_Child(Location);
                        end if;
                     end if;

                     Append_Child(Container => Inst,
                                  Parent    => Location,
                                  New_Item  => Node);
                  end;

                  -- Skip forward to the end of line
                  Posn := Stmt'Length + 1;

                  -- and bug out!
                  return;

               -- Ignore spaces
               when '_' | ' ' => null;

               -- Various syntax errors
               when '.' =>
                  raise Syntax_Error with "Unexpected '.' - no function declared";
               when '=' =>
                  raise Syntax_Error with "Unexpected Synonym assignment";
               when others => raise Syntax_Error with "Invalid character";
            end case;

            Posn := Posn + 1;
         end loop;
      end;

   begin
      Index := Root(Default);

      parse_expression( S, C, Default, Index);

      return Default;
   end parse_Statement;

   function reduce( I: Instructions.Tree ) return Instructions.Tree is
   begin
      
      return I;
   end reduce;

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

   -- Format an instructions tree for printing
   function format ( I: Instructions.tree ) return Statement is
      Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;
      Curs : Instructions.Cursor;
   begin
      if not Instructions.is_Empty(I)
      then
         for Curs in Iterate_Children( Container => I, Parent => Root(I) )
         loop
            SU.Append(Buffer, format_Element(I, Curs));
         end loop;
      end if;

      return SU.To_String(Buffer);
   end;

   function format ( I: Instructions.tree; Curs : Instructions.Cursor ) return Statement is
      Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;
   begin
      if not Instructions.is_Empty(I)
      then
         SU.Append(Buffer, format_Element(I, Curs));
      end if;

      return SU.To_String(Buffer);
   end;

   function format_Element ( I : Instructions.tree; Curs : Instructions.Cursor ) return SU.Unbounded_String is
      Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;
      Node : Element_Record := Instructions.Element(Curs);
      E : Element_Record;
      variables : Boolean;
   begin
      Log(Log_Format, "[" & Element_Type'Image(Node.Element) & ", " & Node.Name & ", Is_Explicit=" & Boolean'Image(Node.Is_Explicit) & "]");

      case Node.Element is
         when L_Expression =>
            SU.Append(Buffer, '(');

            for C in Iterate_Children( Container => I, Parent => Curs )
            loop
               SU.Append(Buffer, format_Element(I, C));
            end loop;

            SU.Append(Buffer, ')');

         when L_Function =>
            SU.Append(Buffer, Node.Name);

            variables := True;
            for C in Iterate_Children( Container => I, Parent => Curs )
            loop
               if variables
               then
                  E := Instructions.Element(C);

                  if E.Element /= L_Variable
                  then
                     variables := False;
                     SU.Append(Buffer, '.');
                  end if;
               end if;

               SU.Append(Buffer, format_Element(I, C));
            end loop;

         when L_Definition =>
            -- put Synonym
            SU.Append(Buffer, Node.Name);
            SU.Append(Buffer, '=');

            -- format(Expression)
            for C in Iterate_Children( Container => I, Parent => Curs )
            loop
               SU.Append(Buffer, format_Element(I, C));
            end loop;

         when L_Variable | L_Synonym =>
            SU.Append(Buffer, Node.Name);

         when L_Comments =>
            declare
               First : Element_Record := First_Child_Element(Root(I));
            begin
               if First.Element /= L_Comments
               then
                  -- add a separator
                  SU.Append(Buffer, " ");
               end if;

               SU.Append(Buffer, Node.Name);
               SU.Append(Buffer, Node.Comments);
            end;

      end case;

      return Buffer;
   end;

   procedure Log(S : String) is
   begin
      if Trace
      then
         Put_Line(".. " & S);
      end if;
   end;

   procedure Log(T : Log_Type; S : String) is
   begin
      if Trace
      then
         case T is
            when Log_Parse =>
               if Trace_Parse then
		  Put_Line("... P-" & S);
	       end if;
            when Log_Reduce =>
               if Trace_Reduce then
		  Put_Line("... R-" & S);
	       end if;
            when Log_Format =>
               if Trace_Format then
		  Put_Line("... F-" & S);
	       end if;
            when others =>
	       raise program_error with "Unexpected log type " & Log_Type'image(T);
	 end case;
      end if;
   end;

   -- Synonyms are stored in alphabetical order
   procedure Add_Synonym ( Source: Instructions.Cursor ) is
      Node : Element_Record := Instructions.Element(Source);
      Before : Instructions.Cursor := No_Element;
      Parent : Instructions.Cursor := Root(Synonyms);
      SE : Element_Record;
   begin
      If Node.Element /= L_Definition then
         raise Program_Error with "Cannot add " & Element_Type'Image(Node.Element) & " as a synonym";
      end if;

      if not(Instructions.Is_Empty(Synonyms)) then
         -- Search for a Synonym of the same name
         for Curs in Iterate_Children( Container => Synonyms, Parent => Root(Synonyms) )
         loop
            SE := Instructions.Element(Curs);
            if SE.Name >= Node.Name then
              Before := Curs;
              exit;
            end if;
         end loop;
      end if;

      Copy_Subtree( Target => Synonyms,
                    Parent => Parent,
                    Before => Before,
                    Source => Source);

      if not(Instructions.Is_Empty(Synonyms)) and then SE.Name = Node.Name then
         Delete_Subtree( Container => Synonyms, Position => Before );
      end if;
   end;

   procedure Remove_Synonym ( S: Statement ) is
      SE : Element_Record;
      Curs  : Instructions.Cursor;
      Found : Boolean := FALSE;
   begin
      Curs := First_Child(Root(Synonyms));

      loop
	 exit when Curs = No_Element;

         SE := Instructions.Element(Curs);
         if SE.Name = S(S'First)
	 then
            Log("Removing " & format(Synonyms, Curs));
	    Delete_Subtree( Container => Synonyms, Position => Curs);

	    Found := TRUE;
	    exit;
	 end if; 

	 Curs := Next_Sibling(Curs);
      end loop;

      if not Found
      then
	 Put_Line(". Synonym " & S(S'First) & " not found");
      end if;
   end;

   procedure List_Synonyms is
   begin
      if not(Instructions.Is_Empty(Synonyms)) then
         for Curs in Iterate_Children( Container => Synonyms, Parent => Root(Synonyms) )
         loop
            Put_Line(format(Synonyms, Curs));
         end loop;
      else
         Put_Line(". empty");
      end if;
   end;

end Lambda;

