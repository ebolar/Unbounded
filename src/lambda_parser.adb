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

with Lambda; use Lambda;

Package body Lambda_Parser is

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
                     Instructions.Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);

                     --   move Cursor and reprocess
                     parse_expression(Stmt, Posn, Inst, Instructions.First_Child(Parent => Curs));

                  else
                     Log(Log_Parse, Indent(Level) & "Variable: " & E);

                     -- Insert(L_Variable)
                     Node := (Element => L_Variable, Name => E, is_Explicit => true);
                     Instructions.Append_Child(Container => Inst,
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
                     Instructions.Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);

                     Posn := Locate(Stmt, Posn, '=');
                     Index := Instructions.Last_Child(Parent => Curs);

                     if next_is( Stmt, Posn, '(')
                     then
                        Posn := Locate(Stmt, Posn, '(');
                        parse_expression(Stmt, Posn, Inst, Index);
                     else
                        Node := (Element => L_Expression, Name => '_', Is_Explicit => false);
                        Instructions.Append_Child(Container => Inst,
                                     Parent    => Index,
                                     New_Item  => Node);

                        Posn := Posn + 1;
                        parse_expression(Stmt, Posn, Inst, Instructions.Last_Child(Parent => Index));
                     end if;

                     Add_Synonym( Source=> Index );

                  else
                     if Instructions.Is_Empty (Inst)
                     then
                        --   Insert(L_Expression)
                        Node := (Element => L_Expression, Name => '_', is_Explicit => false);
                        Instructions.Append_Child(Container => Inst,
                                     Parent    => Curs,
                                     New_Item  => Node);

                        --   move Cursor and reprocess
                        parse_expression(Stmt, Posn, Inst, Instructions.Last_Child(Parent => Curs));

                     else
                        Log(Log_Parse, Indent (Level) & "Synonym: " & E);

                        --   Insert (L_Synonym)
                        Node := (Element => L_Synonym, Name => E, is_Explicit => true);
                        Instructions.Append_Child(Container => Inst,
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
                     Instructions.Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);

                     --   move Cursor and reprocess
                     parse_expression(Stmt, Posn, Inst, Instructions.Last_Child(Parent => Curs));

                  else
                     Node := Instructions.Element(Curs);

                     -- Functions are an implicitly defined container
                     Node := (Element => L_Function, Name => E, is_Explicit => false);
                     Instructions.Append_Child(Container => Inst,
                                  Parent    => Curs,
                                  New_Item  => Node);

                     Index := Instructions.Last_Child(Parent => Curs);

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
                              Instructions.Append_Child(Container => Inst,
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
                           Instructions.Append_Child(Container => Inst,
                                        Parent    => Index,
                                        New_Item  => Node);

                           Index := Instructions.Last_Child(Parent => Index);

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
                  Instructions.Append_Child(Container => Inst,
                               Parent    => Curs,
                               New_Item  => Node);

                  --   move Cursor and parse the sub-expression
                  Posn := Posn + 1;
                  parse_expression(Stmt, Posn, Inst, Instructions.Last_Child(Parent => Curs));

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
                     Location : Instructions.Cursor := Instructions.Root(Inst);
                  begin
                     -- if first child is a symbol definition then append to the definition
                     if not Instructions.Is_Empty(Inst)
                     then
                        First := Instructions.First_Child_Element(Location);
                        if First.Element = L_Definition
                        then
                           Location := Instructions.First_Child(Location);
                        end if;
                     end if;

                     Instructions.Append_Child(Container => Inst,
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
      Index := Instructions.Root(Default);

      parse_expression( S, C, Default, Index);

      return Default;
   end parse_Statement;

end Lambda_Parser;

