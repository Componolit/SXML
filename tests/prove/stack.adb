with Ada.Text_IO;
with SXML.Parser;
with SXML.Serialize;
with SXML.Query;

procedure Stack with
   SPARK_Mode,
   Global => (In_Out => (Ada.Text_IO.File_System, SXML.Serialize.State, SXML.Parser.State))
is
   Input : constant String :=
        "<outer attr1=""attr1val"" attr2=""attr2val"" attr3=""attr3val"">"
      & "  <header1/>"
      & "  <header2 hattr21=""hattr21val""/>"
      & "  <header3>"
      & "    <subhdr31>subhdr31content</subhdr31>"
      & "  </header3>"
      & "  <header4 hattr41=""hattr41val"" hattr42=""hattr42val"" hattr43=""hattr43val"" hattr44=""hattr44val"">"
      & "    <subhdr41 hattr411=""hattr411val""/>"
      & "  </header4>"
      & "  <document>"
      & "    <child1/>"
      & "    <child2/>"
      & "    <child3/>"
      & "  </document>"
      & "</outer>";

   Outer        : SXML.Document_Type (1 .. 1000) := (others => SXML.Null_Node);
   Parse_Result : SXML.Parser.Match_Type;
   Offset       : Natural;

   use type SXML.Result_Type;
   use type SXML.Parser.Match_Type;
begin
   SXML.Parser.Parse (Data     => Input,
                      Document => Outer,
                      Result   => Parse_Result,
                      Offset   => Offset);
   if Parse_Result /= SXML.Parser.Match_OK then
      Ada.Text_IO.Put ("Parse error: >>");
      Ada.Text_IO.Put (Input (Input'First .. Input'First + Offset));
      Ada.Text_IO.Put_Line ("<<");
   end if;

   declare
      Output      : String (1 .. 10000);
      Result      : SXML.Result_Type;
      State, Root : SXML.Query.State_Type;
      Attr        : SXML.Query.State_Type;
      Data        : SXML.Content_Type (1 .. 1000);
      Name        : SXML.Content_Type (1 .. 1000);
      Name_Result : SXML.Result_Type;
      Name_Last   : Natural;
      Last        : Natural;
   begin
      Root := SXML.Query.Init (Outer);
      if not SXML.Query.Is_Open (Root, Outer) then
         Ada.Text_IO.Put_Line ("Invalid document (no opening tag)");
         return;
      end if;

      SXML.Query.Name (Root, Outer, Result, Data, Last);
      if Result /= SXML.Result_OK then
         Ada.Text_IO.Put ("Error getting name: ");
         Ada.Text_IO.Put_Line (Result'Img);
         return;
      end if;

      Ada.Text_IO.Put ("<!-- Name: ");
      Ada.Text_IO.Put_Line (Data (Data'First .. Last));

      Attr := SXML.Query.Attribute (Root, Outer);
      while SXML.Query.State_Result (Attr) = SXML.Result_OK loop
         pragma Loop_Invariant (SXML.Query.Is_Attribute (Attr, Outer));
         pragma Loop_Invariant (SXML.Query.Is_Valid (Attr, Outer));
         pragma Loop_Invariant (SXML.Query.Is_Valid_Value (Attr, Outer));
         SXML.Query.Name (Attr, Outer, Name_Result, Name, Name_Last);
         SXML.Query.Value (Attr, Outer, Result, Data, Last);
         if Name_Result = SXML.Result_OK and Result = SXML.Result_OK then
            Ada.Text_IO.Put ("   ");
            Ada.Text_IO.Put (Name (Name'First .. Name_Last));
            Ada.Text_IO.Put (": ");
            Ada.Text_IO.Put_Line (Data (Data'First .. Last));
         end if;
         Attr := SXML.Query.Next_Attribute (Attr, Outer);
      end loop;

      State := SXML.Query.Path (Root, Outer, "/outer/header4");
      if SXML.Query.State_Result (State) = SXML.Result_OK and then SXML.Query.Is_Open (State, Outer) then
         Attr := SXML.Query.Find_Attribute (State, Outer, "hattr43");
         if SXML.Query.State_Result (Attr) = SXML.Result_OK then
            SXML.Query.Name (Attr, Outer, Name_Result, Name, Name_Last);
            SXML.Query.Value (Attr, Outer, Result, Data, Last);
            if Name_Result = SXML.Result_OK and Result = SXML.Result_OK then
               Ada.Text_IO.Put ("   ");
               Ada.Text_IO.Put (Name (Name'First .. Name_Last));
               Ada.Text_IO.Put (": ");
               Ada.Text_IO.Put_Line (Data (Data'First .. Last));
            end if;
         else
            Ada.Text_IO.Put_Line ("   ERROR: No attribute");
         end if;

         SXML.Query.Attribute (State, Outer, "hattr41", Result, Data, Last);
         if Result = SXML.Result_OK then
            Ada.Text_IO.Put ("   hattr41: ");
            Ada.Text_IO.Put_Line (Data (Data'First .. Last));
         end if;

         if not SXML.Query.Has_Attribute (State, Outer, "hattr42") then
            Ada.Text_IO.Put_Line ("   ERROR: hattr42 not found");
         end if;

      else
         Ada.Text_IO.Put ("   ERROR: ");
         Ada.Text_IO.Put_Line (SXML.Query.State_Result (State)'Img);
      end if;

      State := SXML.Query.Path (Root, Outer, "/outer/header1");
      if SXML.Query.State_Result (State) = SXML.Result_OK and then SXML.Query.Is_Open (State, Outer) then
         State := SXML.Query.Find_Sibling (State, Outer, "header2");
         if SXML.Query.State_Result (State) = SXML.Result_OK then
            State := SXML.Query.Sibling (State, Outer);
            if SXML.Query.State_Result (State) = SXML.Result_OK and then SXML.Query.Is_Open (State, Outer) then
               SXML.Query.Name (State, Outer, Result, Data, Last);
               if Result = SXML.Result_OK then
                  Ada.Text_IO.Put ("   FOUND ");
                  Ada.Text_IO.Put_Line (Data (Data'First .. Last));
                  State := SXML.Query.Child (State, Outer);
                  if SXML.Query.State_Result (State) = SXML.Result_OK and then SXML.Query.Is_Open (State, Outer) then
                     SXML.Query.Name (State, Outer, Result, Data, Last);
                     if Result = SXML.Result_OK then
                        Ada.Text_IO.Put ("   FOUND ");
                        Ada.Text_IO.Put_Line (Data (Data'First .. Last));
                     end if;
                  end if;
               else
                  Ada.Text_IO.Put ("   ERROR: header3 not found - ");
                  Ada.Text_IO.Put_Line (SXML.Query.State_Result (State)'Img);
               end if;
            else
               Ada.Text_IO.Put ("   ERROR: header3 not found - ");
               Ada.Text_IO.Put_Line (SXML.Query.State_Result (State)'Img);
            end if;
         else
            Ada.Text_IO.Put ("   ERROR: header2 not found - ");
            Ada.Text_IO.Put_Line (SXML.Query.State_Result (State)'Img);
         end if;
      end if;

      Ada.Text_IO.Put_Line ("-->");

      SXML.Serialize.To_String (Outer, Output, Offset, Result);
      if Result /= SXML.Result_OK then
         Ada.Text_IO.Put ("Serialization error: ");
         Ada.Text_IO.Put_Line (Result'Img);
         return;
      end if;
      Ada.Text_IO.Put_Line (Output (Output'First .. Output'First + Offset));
   end;

end Stack;
