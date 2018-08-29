package body SXML
   with SPARK_Mode
is
   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Float) return String
   with
      SPARK_Mode => Off
   is
      Value_Img : constant String := Value'Img;
   begin
      if Value >= 0.0
      then
         --  Remove leading space
         return Value_Img (2 .. Value_Img'Last);
      else
         return Value_Img;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Integer) return String
   with
      SPARK_Mode => Off
   is
   begin
      if Value >= 0
      then
         return Value'Img (2 .. Value'Img'Last);
      else
         return Value'Img;
      end if;
   end To_String;

   --------------
   -- Set_Data --
   --------------

   function Set_Data (Data : String;
                      Kind : Kind_Type) return Subtree_Type;

   function Set_Data (Data : String;
                      Kind : Kind_Type) return Subtree_Type
   is
      Result_Len : constant Index_Type := (Data'Length - 1) / Data_Type'Length + 1;
      Index      : Natural := Data'First;
      Chunk_Kind : Kind_Type := Kind;
   begin
      return Result : Subtree_Type (1 .. Result_Len) := (others => Null_Node)
      do
         for I in Result'Range
         loop
            declare
               Offset : constant Natural :=
                 (if Data'Last - Index > Data_Type'Length then
                     Data_Type'Length - 1
                  else
                     Data'Last - Index);
               D  : Data_Type := (others => Character'Val (0));
            begin
               D (Data_Type'First .. Data_Type'First + Offset) :=
                 Data (Index .. Index + Offset);
               Result (I) := Node_Type'(Kind   => Chunk_Kind,
                                        Length => Offset + 1,
                                        Data   => D);
            end;
            Chunk_Kind := Kind_Data;
            Index := Index + Data_Type'Length;
         end loop;
      end return;
   end Set_Data;

   ----------
   -- Open --
   ----------

   function Open (Name : String) return Subtree_Type
   is
   begin
      return Set_Data (Name, Kind_Element_Open);
   end Open;

   -----------
   -- Close --
   -----------

   function Close (Name : String) return Subtree_Type
   is
   begin
      return Set_Data (Name, Kind_Element_Close);
   end Close;

   -------
   -- E --
   -------

   function E (Name       : String;
               Children   : Subtree_Type := Null_Tree) return Subtree_Type
   is
      O : constant Subtree_Type := Open (Name);
      C : constant Subtree_Type := Close (Name);
      Index : Index_Type := 1;
   begin
      return Result : Subtree_Type (1 .. Children'Length + O'Length + C'Length) :=
        (others => Null_Node)
      do
         for N of O
         loop
            Result (Index) := N;
            Index := Index + 1;
         end loop;

         for N of Children
         loop
            Result (Index) := N;
            Index := Index + 1;
         end loop;

         for N of C
         loop
            Result (Index) := N;
            Index := Index + 1;
         end loop;
      end return;
   end E;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Integer) return Subtree_Type
   is
   begin
      return A (Name, To_String (Value));
   end A;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Float) return Subtree_Type
   is
   begin
      return A (Name, To_String (Value));
   end A;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : String) return Subtree_Type
   is
   begin
      return Set_Data (Name,  Kind_Attr_Name) &
             Set_Data (Value, Kind_Attr_Data);
   end A;

   --------------
   -- Data_Len --
   --------------

   function Data_Len (Node : Node_Type) return Natural is
     (if Node.Length = 0 then Node.Data'Length else Node.Length);

   --------------
   -- Node_Len --
   --------------

   function Node_Len (Node : Node_Type) return Natural
   is
     (case Node.Kind is
         when Kind_Invalid       => 0,
         when Kind_Element_Open  => 2 + Data_Len (Node),
         when Kind_Element_Close => 3 + Data_Len (Node),
         when Kind_Attr_Name     => 2 + Data_Len (Node),
         when Kind_Attr_Data     => 2 + Data_Len (Node),
         when Kind_Data          => Data_Len (Node));

   --------------
   -- Text_Len --
   --------------

   function Text_Len (Tree : Subtree_Type) return Natural;

   function Text_Len (Tree : Subtree_Type) return Natural
   is
      Result  : Natural := 0;
      Tmp     : Natural;
   begin
      for E of Tree
      loop
         exit when E = Null_Node;
         Tmp := Node_Len (E);
         if Tmp > Natural'Last - Result
         then
            Result := 0;
            exit;
         end if;
         Result := Result + Tmp;
      end loop;
      return Result;
   end Text_Len;

   ---------------
   -- To_String --
   ---------------

   function To_String (Tree : Subtree_Type) return String
   is
      Position : Natural := 0;
      Is_Open  : Boolean := False;

      function In_Range (Buffer : String;
                         Length : Natural) return Boolean;

      function In_Range (Buffer : String;
                         Length : Natural) return Boolean
      is
        (Buffer'First <= Natural'Last - Position and then
         Buffer'First + Position <= Natural'Last - Length and then
         Buffer'First + Position + Length - 1 <= Buffer'Last);

      procedure Append (Result : in out String;
                        Data   :        String)
        with
          Global => (In_Out => Position),
        Pre    => Data'Length > 0 and
        Position <= Result'Length - Data'Length;

      procedure Append (Result : in out String;
                        Data   :        String)
      is
      begin
         for I in 0 .. Data'Length - 1
         loop
            Result (Result'First + Position + I) := Data (Data'First + I);
         end loop;
         Position := Position + Data'Length;
      end Append;

      TL : constant Natural := Text_Len (Tree);
      Invalid : constant String := "";
   begin
      if TL = 0
      then
         return Invalid;
      end if;

      declare
         Result : String (1 .. TL) := (others => Character'Val (0));
      begin
         Fill_Result :
         for E of Tree
         loop
            exit Fill_Result when E = Null_Node;
            case E.Kind
            is
               when Kind_Element_Open =>
                  if Is_Open
                  then
                     if not In_Range (Result, 1)
                     then
                        return Invalid;
                     end if;
                     Append (Result, ">");
                  end if;
                  Is_Open := True;
                  begin
                     if not In_Range (Result, 1 + E.Length)
                     then
                        return Invalid;
                     end if;
                     Append (Result, "<" & E.Data (1 .. E.Length));
                  end;
               when Kind_Element_Close =>
                  if Is_Open
                  then
                     Is_Open := False;
                     if not In_Range (Result, 1)
                     then
                        return Invalid;
                     end if;
                     Append (Result, ">");
                  end if;
                  if not In_Range (Result, 3 + E.Length)
                  then
                     return Invalid;
                  end if;
                  Append (Result, "</" & E.Data (1 .. E.Length));
                  Is_Open := True;
               when Kind_Attr_Name =>
                  Append (Result, " " & E.Data (1 .. E.Length) & "=");
               when Kind_Attr_Data =>
                  Append (Result, """" & E.Data (1 .. E.Length) & """");
               when Kind_Data =>
                  Append (Result, E.Data (1 .. E.Length));
               when Kind_Invalid =>
                  exit Fill_Result;
            end case;
         end loop Fill_Result;

         if Is_Open
         then
            Is_Open := False;
            if not In_Range (Result, 1)
            then
               return Invalid;
            end if;
            Append (Result, ">");
         end if;
         return Result;
      end;
   end To_String;

end SXML;
