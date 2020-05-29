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

Package body Lambda_Reducer is

   function Reduce( I: Instructions.Tree ) return Instructions.Tree is
      Inst : Instructions.Tree := I;
   begin
      Log("Reduce ",  Inst);
      
      -- optimise the tree starting at the root
      Optimise(Inst, Instructions.Root(Inst));

      return Inst;
   end reduce;

   -- Expression Optimiser 
   -- - ((X)) => (X)
   -- - X() => X
   procedure Optimise( I: in out Instructions.Tree ; Curs : Instructions.Cursor ) is
      Node : Element_Record;
      Sub_Node : Element_Record;
      Location : Instructions.Cursor;
      Expression : Instructions.Cursor;
      Sub_Expression : Instructions.Cursor;
   begin
      -- OK this is starting to get ugly.  Urgently need an Instructions ADT.
      if not Instructions.is_Root(Curs) then
         Log(Log_Reduce, "Optimise" & Indent(Natural(Instructions.Depth(Curs))), I, Curs);
      end if;
 
      if not Instructions.is_Empty(I) 
         and not Instructions."=" (Curs, Instructions.No_Element)
      then
         Location := Instructions.First_Child( Curs );

         -- for all children of Curs
         loop
            exit when Instructions."=" (Location, Instructions.No_Element);

            Node := Instructions.Element( Location );
            case Node.Element is
               -- Function or Symbol declaration - just reprocess at the next level
               when L_Function | L_Definition => 
                  Optimise( I, Location );
                  Location := Instructions.Next_Sibling( Location );

               -- Expression optimisation
               when L_Expression =>
                  -- We are about to tamper with the tree's cursors 
                  -- Save the locations we care about 
                  Sub_Expression := Instructions.First_Child( Location );
                  Expression := Location;
                  Location := Instructions.Next_Sibling( Location );

                  -- If no children then
                  if Instructions."=" (Sub_Expression, Instructions.No_Element)
                  then
                     -- delete this Expression
                     Instructions.Delete_Leaf( I, Expression );

                  -- If one child and child = expression
                  elsif Instructions.Child_Count( Expression ) = 1
                  then
                     Sub_Node := Instructions.Element( Sub_Expression );
                     if Sub_Node.Element = L_Expression
                     then
                        -- First optimise the SubExpression
                        Optimise( I, Sub_Expression );

                        -- Then copy SubExpression before Expression
                        Instructions.Copy_Subtree( Target => I,
                                                   Parent => Curs,
                                                   Before => Expression,
                                                   Source => Sub_Expression);

                        -- and delete the Expression
                        Instructions.Delete_Subtree( I, Expression );

                     else 
                        Optimise( I, Expression );
                     end if;
                  
                  -- Otherwise
                  else
                     -- Just optimise the expression
                     Optimise( I, Expression );
                  end if;
               when others =>
                  Location := Instructions.Next_Sibling( Location );
            end case;

         end loop; 
      end if;
   end Optimise;

end Lambda_Reducer;

