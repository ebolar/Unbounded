-- Lambda Calculus interpreter
-- ---------------------------
-- Parses and reduces Lamdba Calculus statements.
--
-- Source:
-- lambda         - [This file] definitions and helper functions
-- lambda_REPL    - REPL and command line parsers
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

Package body Lambda is

   -- Format all the elements in an instructions tree for printing
   -- Public function
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

   -- Format all the elements in an instructions sub-tree for printing
   -- Public function
   function format ( I: Instructions.tree; Curs : Instructions.Cursor ) return Statement is
      Buffer : SU.Unbounded_String := SU.Null_Unbounded_String;
   begin
      if not Instructions.is_Empty(I)
      then
         SU.Append(Buffer, format_Element(I, Curs));
      end if;

      return SU.To_String(Buffer);
   end;

   -- Recursively format all of the instruction elements
   -- Private function
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

   -- Add a Synonym to the list
   --
   -- Nb: Synonyms are stored in alphabetical order
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

   -- Remove a Synonym from the list
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

   -- List all Synonyms
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

   -- General logging, eg for the REPL
   procedure Log(S : String) is
   begin
      if Trace
      then
         Put_Line(".. " & S);
      end if;
   end;

   -- Granular logging
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

end Lambda;

