with Ada.Text_IO;
with SXML.Parser;
with SXML.Serialize;
with SXML.Query;
with SXML.Generator; use SXML.Generator;

procedure Complex with
   SPARK_Mode,
   Global => (In_Out => (Ada.Text_IO.File_System, SXML.Serialize.State, SXML.Parser.State))
is
   Input : constant String :=
        "<document>"
      &   "<child1></child1>"
      &   "<child2></child2>"
      &   "<child3></child3>"
      & "</document>";

   Inner        : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);
   Parse_Result : SXML.Parser.Match_Type;
   Offset       : Natural;

   use type SXML.Result_Type;
   use type SXML.Content_Type;
   use type SXML.Parser.Match_Type;
begin
   SXML.Parser.Parse (Data     => Input,
                      Document => Inner,
                      Result   => Parse_Result,
                      Offset   => Offset);
   if Parse_Result /= SXML.Parser.Match_OK then
      Ada.Text_IO.Put_Line ("Parse error: >>" & Input (Input'First .. Input'First + Offset) & "<<");
   end if;

   declare
      Output : String (1 .. 10000);
      Result : SXML.Result_Type;

      Outer : constant SXML.Document_Type := E ("outer",

                                                A ("attr1", "attr1val")
                                                + A ("attr2", "attr2val")
                                                + A ("attr3", "attr3val"),

                                                E ("header1")
                                                + E ("header2",
                                                     A ("hattr21", "hattr21val"))
                                                + E ("header3",
                                                     E ("subhdr31", C ("subhdr31content")))
                                                + E ("header4",
                                                     A ("hattr41", "hattr41val")
                                                     + A ("hattr42", "hattr42val")
                                                     + A ("hattr43", "hattr43val")
                                                     + A ("hattr44", "hattr44val"),
                                                     E ("subhdr41", A ("hattr411", "hattr411val")))
                                                + Inner);
      State, Root : SXML.Query.State_Type;
      Attr        : SXML.Query.State_Type;
      Data        : SXML.Content_Type (1..1000);
      Name        : SXML.Content_Type (1..1000);
      Name_Result : SXML.Result_Type;
      Name_Last   : Natural;
      Last        : Natural;
      use type SXML.Offset_Type;
   begin
      Root := SXML.Query.Init (Outer);
      if not SXML.Query.Is_Open (Root, Outer) then
         Ada.Text_IO.Put_Line ("Invalid document (no opening tag)");
         return;
      end if;

      SXML.Query.Name (Root, Outer, Result, Data, Last);
      if Result /= SXML.Result_OK then
         Ada.Text_IO.Put_Line ("Error getting name: " & Result'Img);
         return;
      end if;

      Ada.Text_IO.Put_Line ("<!-- Name: " & Data (Data'First .. Last));

      Attr := SXML.Query.Attribute (Root, Outer);
      while Attr.Result = SXML.Result_OK loop
         pragma Loop_Invariant (SXML.Query.Is_Attribute (Attr, Outer));
         pragma Loop_Invariant (SXML.Query.Is_Valid (Attr, Outer));
         pragma Loop_Invariant (SXML.Query.Is_Valid_Value (Attr, Outer));
         SXML.Query.Name (Attr, Outer, Name_Result, Name, Name_Last);
         SXML.Query.Value (Attr, Outer, Result, Data, Last);
         if Name_Result = SXML.Result_OK and Result = SXML.Result_OK then
            Ada.Text_IO.Put_Line ("   " & Name (Name'First .. Name_Last) & ": " & Data (Data'First .. Last));
         end if;
         Attr := SXML.Query.Next_Attribute (Attr, Outer);
      end loop;

      State := SXML.Query.Path (Root, Outer, "/outer/header4");
      if State.Result = SXML.Result_OK and then SXML.Query.Is_Open (State, Outer) then
         Attr := SXML.Query.Find_Attribute (State, Outer, "hattr43");
         if Attr.Result = SXML.Result_OK then
            SXML.Query.Name (Attr, Outer, Name_Result, Name, Name_Last);
            SXML.Query.Value (Attr, Outer, Result, Data, Last);
            if Name_Result = SXML.Result_OK and Result = SXML.Result_OK then
               Ada.Text_IO.Put_Line ("   " & Name (Name'First .. Name_Last) & ": " & Data (Data'First .. Last));
            end if;
         else
            Ada.Text_IO.Put_Line ("   ERROR: No attribute");
         end if;

         SXML.Query.Attribute (State, Outer, "hattr41", Result, Data, Last);
         if Result = SXML.Result_OK then
            Ada.Text_IO.Put_Line ("   hattr41: " & Data (Data'First .. Last));
         end if;

         if not SXML.Query.Has_Attribute (State, Outer, "hattr42") then
            Ada.Text_IO.Put_Line ("   ERROR: hattr42 not found");
         end if;

      else
         Ada.Text_IO.Put_Line ("   ERROR: " & State.Result'Img);
      end if;

      State := SXML.Query.Path (Root, Outer, "/outer/header1");
      if State.Result = SXML.Result_OK and then SXML.Query.Is_Open (State, Outer) then
         State := SXML.Query.Find_Sibling (State, Outer, "header2");
         if State.Result = SXML.Result_OK then
            State := SXML.Query.Sibling (State, Outer);
            if State.Result = SXML.Result_OK and then SXML.Query.Is_Open (State, Outer) then
               SXML.Query.Name (State, Outer, Result, Data, Last);
               if Result = SXML.Result_OK then
                  Ada.Text_IO.Put_Line ("   FOUND " & Data (Data'First .. Last));
                  State := SXML.Query.Child (State, Outer);
                  if State.Result = SXML.Result_OK and then SXML.Query.Is_Open (State, Outer) then
                     SXML.Query.Name (State, Outer, Result, Data, Last);
                     if Result = SXML.Result_OK then
                        Ada.Text_IO.Put_Line ("   FOUND " & Data (Data'First .. Last));
                     end if;
                  end if;
               else
                  Ada.Text_IO.Put_Line ("   ERROR: header3 not found - " & State.Result'Img);
               end if;
            else
               Ada.Text_IO.Put_Line ("   ERROR: header3 not found - " & State.Result'Img);
            end if;
         else
            Ada.Text_IO.Put_Line ("   ERROR: header2 not found - " & State.Result'Img);
         end if;
      end if;

      Ada.Text_IO.Put_Line ("-->");

      SXML.Serialize.To_String (Outer, Output, Offset, Result);
      if Result /= SXML.Result_OK then
         Ada.Text_IO.Put_Line ("Serialization error: " & Result'Img);
         return;
      end if;
      Ada.Text_IO.Put_Line (Output (Output'First .. Output'First + Offset));
   end;

end Complex;
