package body SXML
   with SPARK_Mode
is

   -------------
   -- Is_Attr --
   -------------

   function Is_Attr (Node : Node_Type) return Boolean
   is
   begin
      return
         Node.Kind = Kind_Attr_Integer or
         Node.Kind = Kind_Attr_Float or
         Node.Kind = Kind_Attr_String;
   end Is_Attr;

   -------------
   -- To_Name --
   -------------

   function To_Name (Name : String) return Name_Type
   with
      Pre => Is_Valid (Name);

   function To_Name (Name : String) return Name_Type
   is
      Result : Name_Type := (others => Character'Val (0));
   begin
      for I in Name'Range
      loop
         Result (I) := Name (I);
      end loop;
      return Result;
   end To_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (Name : Name_Type) return String
   with
      Post => To_String'Result'Length <= Name_Type'Length;

   function To_String (Name : Name_Type) return String
   is
      Len : Natural := 0;
   begin
      loop
         pragma Loop_Invariant (Len <= Name'Last);
         exit when Len >= Name'Last or else Name (Len + 1) = Character'Val (0);
         Len := Len + 1;
      end loop;
      return String (Name (1 .. Len));
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Float) return String
   with
      SPARK_Mode => Off
   is
      Value_Img : String := Value'Img;
   begin
      if Value >= 0.0
      then
         -- Remove leading space
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

   -------
   -- E --
   -------

   function E (Name       : String;
               Children   : Subtree_Type := Null_Tree) return Subtree_Type
   is
      Index : Index_Type;
   begin
      return Result : Subtree_Type (1 .. Children'Length + 2) := (others => Null_Node)
      do
         Result (Result'First) := (Kind => Kind_Element_Open, Name => To_Name (Name));
         Index := 2;
         for Child of Children
         loop
            Result (Index) := Child;
            Index := Index + 1;
         end loop;
         Result (Result'Last) := (Kind => Kind_Element_Close, Name => To_Name (Name));
      end return;
   end E;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Integer) return Subtree_Type
   is
   begin
      return (1 => (Kind          => Kind_Attr_Integer,
                    Name          => To_Name (Name),
                    Integer_Value => Value));
   end A;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Float) return Subtree_Type
   is
   begin
      return (1 => (Kind        => Kind_Attr_Float,
                    Name        => To_Name (Name),
                    Float_Value => Value));
   end A;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : String) return Subtree_Type
   is
   begin
      return (1 => (Kind         => Kind_Attr_String,
                    Name         => To_Name (Name),
                    String_Value => To_Name (Value)));
   end A;

   --------------
   -- Node_Len --
   --------------

   function Node_Len (Node : Node_Type) return Natural
   is
     (case Node.Kind is
         when Kind_Invalid       => 0,
         when Kind_Element_Open  => 2 + To_String (Node.Name)'Length,
         when Kind_Element_Close => 3 + To_String (Node.Name)'Length,
         when Kind_Attr_Integer  => 4 + To_String (Node.Name)'Length + To_String (Node.Integer_Value)'Length,
         when Kind_Attr_Float    => 4 + To_String (Node.Name)'Length + To_String (Node.Float_Value)'Length,
         when Kind_Attr_String   => 4 + To_String (Node.Name)'Length + To_String (Node.String_Value)'Length);

   --------------
   -- Text_Len --
   --------------

   function Text_Len (Tree : Subtree_Type) return Natural
   is
      Result  : Natural := 0;
      Tmp     : Natural;
   begin
      for E of Tree
      loop
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
   begin
      if TL = 0
      then
         return "";
      end if;

      return Result : String (1 .. TL) := (others => Character'Val (0))
      do
         Fill_Result :
         for E of Tree
         loop
            case E.Kind
            is
               when Kind_Element_Open =>
                  if Is_Open
                  then
                     Append (Result, ">");
                  end if;
                  Is_Open := True;
                  Append (Result, "<" & To_String (E.Name));
               when Kind_Element_Close =>
                  if Is_Open
                  then
                     Is_Open := False;
                     Append (Result, ">");
                  end if;
                  Append (Result, "</" & To_String (E.Name) & ">");
               when Kind_Attr_Integer =>
                  Append (Result, " " & To_String (E.Name) & "=""" & To_String (E.Integer_Value) & """");
               when Kind_Attr_String =>
                  Append (Result, " " & To_String (E.Name) & "=""" & To_String (E.String_Value) & """");
               when Kind_Attr_Float =>
                  Append (Result, " " & To_String (E.Name) & "=""" & To_String (E.Float_Value) & """");
               when Kind_Invalid =>
                  exit Fill_Result;
            end case;
         end loop Fill_Result;
      end return;
   end To_String;

end SXML;