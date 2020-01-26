with Ada.Text_IO; use Ada.Text_IO;

Package body Lambda is

   -- are there any Ada parsers I can beg borrow or steal?
   function parse( S: Statement ) return Instructions is

      Default : Instructions;
      L_Element : Lambda_Expression := (ID => "Fred ");  -- should fail; needs to be abstract only
      L_Variable : Lambda_Variable := (ID => "Crap ",
                          Name => 'x',
                          IsBound => FALSE);
      L_Function : Lambda_Function := (ID => "Stuff",
                          Name => 'I',
                          Value => "I=fn x.x                                          ");
      L_Application : Lambda_Application := (ID => "Mess ",
                             Value => "II                                                ");

   begin

      Put("Parsing ["); Put(S); Put_Line("]");
      Put_Line("> checking data structures");

      if S = "" then
         return Default;
      end if;

      for E in 1..S'Length loop
         Put( ".");
      end loop;

      Put_Line("");

      -- Checking datastructures
      Default.Append(L_Element);
      Default.Append(Lambda_Expression (L_Variable));
      Default.Append(Lambda_Expression (L_Function));
      Default.Append(Lambda_Expression (L_Application));

      Put_Line("> Found " & Ada.Containers.Count_Type'Image(Default.Length) & " Instructions");

      return Default;
   end parse;

   function reduce( I: Instructions ) return Instructions is
      Default : Instructions;
   begin
      if I.Is_Empty then
         raise Internal_Error;
      end if;

      return Default;
   end reduce;

   function format( I: Instructions ) return Statement is
      Default : Statement := "";
   begin
      if I.Is_Empty then
         raise Internal_Error;
      end if;

      return Default;
   end format;

   procedure Put_Statement( S: in Statement) is
   begin
      if S = "" then
         raise Syntax_Error;
      end if;
   end Put_Statement;

   function Get_Statement return Statement is
      Default : Statement := "";
   begin
      return Default;
   end Get_Statement;

end Lambda;
