-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;  -- do I need this?
with Ada.Strings.Unbounded;
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
	       -- Log("-> " & format(I));

               -- reduce the line
               I := reduce(I);

               -- print the line
	       Put_Line("-> " & format(I));
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

   -- Parse commands from the REPL
   function parse_Commands( S: Statement ) return Boolean is
      X : String := To_Upper(trim(S, Both));
   begin
      -- Check for commands
      -- : "TRACE" : Display tracing level
      -- : "TRACE-ON" : Verbose tracing
      -- : "TRACE-OFF" : Normal
      -- : "HELP" : Print the help/usage message
      -- : "EXIT" | "QUIT" : Exit
      -- : "LS" : List saved expressions

      if X = "EXIT" OR X = "QUIT"
      then
         Log(". Exit");
         raise Ada.IO_Exceptions.END_ERROR;

      elsif X = "LS"
      then
         Log(". List saved expressions");
	 List_Synonyms;
         return True;

      elsif X = "RM"
      then
	 Log(". Remove synonym");
	 return True;

      elsif X = "TRACE-ON"
      then
         Put_Line(". Trace on");
         Trace := True;
         return True;

      elsif X = "TRACE-OFF"
      then
         Put_Line(". Trace off");
         Trace := False;
         return True;

      elsif X = "TRACE"
      then
         if Trace
         then
            Put_Line(". Trace on");
         else
            Put_Line(". Trace off");
         end if;
         return True;

      elsif X = "HELP"
      then
         Put_Line("EXIT | QUIT : Exit Unbounded");
         Put_Line("HELP        : Display this message");
         Put_Line("LS          : List all saved expressions");
         Put_Line("RM <symbol> : Delete an expression");
         Put_Line("TRACE       : Display the tracing level");
         Put_Line("TRACE-ON    : Enable verbose tracing");
         Put_Line("TRACE-OFF   : Normal output");
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
   --              : [_| ]   : Spaces.  Ignore these.
   --
   function parse_Statement( S: Statement ) return Instructions.Tree is
      Default : Instructions.Tree;
      Index : Instructions.Cursor;
      -- Node : Element;

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

      -- TBD: break these out into multiple functions
      -- parse_expression()
      -- parse_function()
      -- parse_synonym()
      --
      -- Cleanup the code - using Next_Is and Locate helper functions.
      procedure parse_expression( Stmt : Statement; Posn : in out Natural; Inst : in out Instructions.Tree; Curs : Instructions.Cursor ) is
         E : Character;
         Node : Element;
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

	 -- TBD - pick one of these.
	 -- The clean functional style
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

	 -- Results in nicer looking code, but with side effects!!!
	 procedure Locate(Stmt : Statement; Posn : in out Natural; Expected : Character) is
	    E : Character;
	 begin
            while Posn <= Stmt'Length loop
               E := Stmt(Posn);
	       if E = Expected then
		       exit;
	       else
		       Posn := Posn + 1;
	       end if;
	    end loop;
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
                     -- Log("... " & integer'image(Level) & Indent(Level) & "Variable: " & E);
                     Log("... " & Indent(Level) & "Variable: " & E);

		     -- Insert(L_Variable)
                     Node := (Element => L_Variable, Name => E, is_Explicit => true);
	             Append_Child(Container => Inst,
	                          Parent    => Curs,
		                  New_Item  => Node);
		  end if;

               when Synonym_Type =>
                  -- if empty tree and next character is '=' then
		  if Instructions.Is_Empty(Inst) and next_is( Stmt, Posn, '=') then
                     --   this is a declaration.  
		     --   Insert (L_Definition (L_Symbol, L_Expression ...)
                     Log("... " & Indent (Level) & "Synonym: " & E);
                     Log("... " & Indent (Level) & "= ");
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

		     -- Insert into the Synonym list
		     Add_Synonym( Source => Index);

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
                        Log("... " & Indent (Level) & "Synonym: " & E);

		        --   Insert (L_Synonym)
                        Node := (Element => L_Synonym, Name => E, is_Explicit => true);
	                Append_Child(Container => Inst,
	                             Parent    => Curs,
		                     New_Item  => Node);
		     end if;
		  end if;

               when '?' | '&' | '\' =>
		  -- Functions

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

                     -- Log("... " & integer'image(Level) & Indent (Level) & "Function - Variables");
                     Log("... " & Indent (Level) & "Function - Variables");
                     Level := Level + 1;

		     Posn := Posn + 1;
                     while Posn <= Stmt'Length loop
                        E := Stmt(Posn);
			Posn := Posn + 1;

			exit when E = '.';

			case E is
                           when Name_Type =>
                              -- Log("... " & integer'image(Level) & Indent(Level) & "Variable: " & E);
                              Log("... " & Indent(Level) & "Variable: " & E);

		              -- Insert(L_Variable)
                              Node := (Element => L_Variable, Name => E, is_Explicit => true);
	                      Append_Child(Container => Inst,
	                                   Parent    => Index,
		                           New_Item  => Node);
                           when others => raise Syntax_Error with "Malformed function declaration";
			end case;
                     end loop;
                     -- Log("... " & integer'image(Level) & Indent (Level - 1) & "Function - Expression");
                     Log("... " & Indent (Level - 1) & "Function - Expression");

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

               when '.' =>
                  raise Syntax_Error with "Unexpected '.' - no function declared";

               when '(' =>
                  -- Log("... " & integer'image(Level) & Indent (Level) & "(");
                  Log("... " & Indent (Level) & "(");

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
                        -- Log("... " & integer'image(Level) & Indent (Level) & ")");
                        Log("... " & Indent (Level) & ")");
                     else
                        -- If we are dealing with an implicitly defined container
                        -- and we encounter a ')',
                        -- it belongs to the enclosing expression and not this one!
                        -- Log("... " & integer'image(Level) & Indent (Level) & ".");
                        Log("... " & Indent (Level) & ".");
		        Posn := Posn - 1;
		     end if;

		     return;
                  exception
                     when Constraint_Error =>
                        raise Syntax_Error with "Unmatched ')'";
                  end;

               when '=' =>
		  raise Syntax_Error with "Unexpected Synonym assignment";
               when '_' | ' ' => null;
               when '#' =>
                  -- Log("... " & integer'image(Level) & Indent (Level) & "#");
                  Log("... " & Indent (Level) & "#");

		  -- Skip forward to the end of line
		  Posn := Stmt'Length + 1;

		  -- and bug out!
		  return;
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
   function Get_Statement return Statement is
      Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;
   begin
      Buffer := Ada.Text_IO.Unbounded_IO.Get_Line;

      return Ada.Strings.Fixed.Head(Source => SU.To_String(Buffer),
                                    Count => Max_Statement_Length,
                                    Pad => ' ');
   end Get_Statement;

   function format_Element ( I : Instructions.tree; Curs : Instructions.Cursor ) return SU.Unbounded_String is
      Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;
      Node : Element := Instructions.Element(Curs);
      E : Element;
      variables : Boolean;
   begin
      case Node.Element is
         when L_Expression =>
            -- Log("... Expression");
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

      end case;

      return Buffer;
   end;

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

   procedure Log(S : String) is
   begin
      if Trace
      then
         Put_Line(S);
      end if;
   end;

   -- Synonyms are stored in alphabetical order
   procedure Add_Synonym ( Source: Instructions.Cursor ) is
      Node : Element := Instructions.Element(Source);
      Before : Instructions.Cursor := No_Element;
      Parent : Instructions.Cursor := Root(Synonyms);
      SE : Element;
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
   begin 
      null;
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

