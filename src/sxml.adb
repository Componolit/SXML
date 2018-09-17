--  with SXML.Debug;
with SXML.Stack;

package body SXML
   with SPARK_Mode
is
   Null_Open_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Element_Open,
                Length         => 0,
                Next           => Invalid_Relative_Index,
                Data           => Null_Data,
                Attributes     => Invalid_Relative_Index,
                Children       => Invalid_Relative_Index,
                Siblings       => Invalid_Relative_Index);

   Null_Data_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Data,
                Length         => 0,
                Next           => Invalid_Relative_Index,
                Data           => Null_Data);

   Null_Attribute_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Attribute,
                Length         => 0,
                Next           => Invalid_Relative_Index,
                Next_Attribute => Invalid_Relative_Index,
                Data           => Null_Data,
                Value          => Invalid_Relative_Index);

   Null_Content_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Content,
                Length         => 0,
                Next           => Invalid_Relative_Index,
                Data           => Null_Data,
                Attributes     => Invalid_Relative_Index,
                Children       => Invalid_Relative_Index,
                Siblings       => Invalid_Relative_Index);

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (D : String) return Offset_Type
   is ((Offset_Type (D'Length + Data_Type'Length - 1)) / Offset_Type (Data_Type'Length));

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Subtree : in out Subtree_Type;
                         Offset  : Offset_Type;
                         Name    : String)
   is
      Position : Natural := 0;
      Len      : Natural;
      NE       : constant Offset_Type := Num_Elements (Name);
   begin
      if NE = 0
      then
         return;
      end if;

      for I in Index_Type range
        Add (Subtree'First, Offset) .. Add (Add (Subtree'First, Offset), NE) - 1
      loop
         if Name'Length - Position > Subtree (I).Data'Length
         then
            Len := Subtree (I).Data'Length;
            Subtree (I).Next := 1;
         else
            Len := Name'Length - Position;
         end if;
         Subtree (I).Data (1 .. Len) :=
           Name (Name'First + Position .. Name'First + Position + Len - 1);
         Subtree (I).Length := Length_Type (Len);
         Position := Position + Len;
      end loop;
   end Put_String;

   ----------
   -- Open --
   ----------

   function Open (Name : String) return Subtree_Type
   is
      Result : Subtree_Type (1 .. Add (1, Num_Elements (Name))) :=
        (1      => Null_Open_Element,
         others => Null_Data_Element);
   begin
      Put_String (Result, 0, Name);
      return Result;
   end Open;

   -----------------
   -- Put_Content --
   -----------------

   procedure Put_Content (Subtree : in out Subtree_Type;
                          Offset  : Offset_Type;
                          Value   : String)
   is
      Start : constant Index_Type := Add (Subtree'First, Offset);
   begin
      Subtree (Start) := Null_Content_Element;
      Subtree (Start + 1 .. Add (Start, Num_Elements (Value) - 1)) := (others => Null_Data_Element);
      Put_String (Subtree, Offset, Value);
   end Put_Content;

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (Name   : String;
                        Data   : String;
                        Offset : in out Offset_Type;
                        Output : out Subtree_Type)
   is
      Name_Elements : constant Offset_Type := Num_Elements (Name);
      Data_Elements : constant Offset_Type := Num_Elements (Data);
      Start         : constant Index_Type := Add (Output'First, Offset);
   begin
      Output (Start) := Null_Attribute_Element;
      Output (Start + 1 .. Add (Add (Start, Name_Elements), Data_Elements) - 1) :=
        (others => Null_Data_Element);

      Put_String (Output, Offset, Name);
      Output (Start).Next  := (if Name_Elements > 1 then 1 else Invalid_Relative_Index);
      Output (Start).Value := Relative_Index_Type (Name_Elements);
      Put_String (Output, Offset + Name_Elements, Data);
      Offset := Offset + Name_Elements + Data_Elements;
   end Attribute;

   ---------
   -- Put --
   ---------

   procedure Put (Value    : String;
                  Data     : in out String;
                  Position : in out Integer);

   procedure Put (Value    : String;
                  Data     : in out String;
                  Position : in out Integer)
   is
   begin
      if Position < 0 or else
         Data'Last - Position < Value'Length
      then
         Position := -1;
         return;
      end if;

      Data (Data'First + Position .. Data'First + Position + Value'Length - 1) := Value;
      Position := Position + Value'Length;
   end Put;

   -----------------
   -- Put_Escaped --
   -----------------

   procedure Put_Escaped (Doc      : Subtree_Type;
                          Start    : Index_Type;
                          Data     : in out String;
                          Position : in out Integer);

   procedure Put_Escaped (Doc      : Subtree_Type;
                          Start    : Index_Type;
                          Data     : in out String;
                          Position : in out Integer)
   is
      procedure Put_Escaped_Char (Char : Character;
                                  D    : in out String;
                                  P    : in out Natural);

      procedure Put_Escaped_Char (Char : Character;
                                  D    : in out String;
                                  P    : in out Natural)
      is
      begin
         case Char is
            when '"' => Put ("&quot;", D, P);
            when ''' => Put ("&apos;", D, P);
            when '&' => Put ("&amp;", D, P);
            when '>' => Put ("&gt;", D, P);
            when '<' => Put ("&lt;", D, P);
            when others => Put ("" & Char, D, P);
         end case;
      end Put_Escaped_Char;

      N   : Node_Type;
      Pos : Index_Type := Start;
   begin

      loop
         N := Doc (Pos);
         for C of N.Data (1 .. Natural (N.Length))
         loop
            Put_Escaped_Char (C, Data, Position);
            if Position < 0
            then
               return;
            end if;
         end loop;
         exit when N.Next = Invalid_Relative_Index;
         Pos := Add (Pos, N.Next);
      end loop;
   end Put_Escaped;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Doc   : Subtree_Type;
                        Start : Offset_Type;
                        Level : Natural := Get_String_Depth) return String
   is
      N : constant Node_Type := Doc (Add (Doc'First, Start));
   begin
      if Level = 0
      then
         return "OVERFLOW";
      end if;

      return N.Data (1 .. Natural (N.Length)) &
         (if N.Next /= Invalid_Relative_Index
          then Get_String (Doc, Add (Start, N.Next), Level - 1)
          else "");
   end Get_String;

   ---------------
   -- Serialize --
   ---------------

   procedure Serialize (Doc      : Subtree_Type;
                        Start    : Index_Type;
                        Data     : in out String;
                        Position : in out Integer);

   procedure Serialize (Doc      : Subtree_Type;
                        Start    : Index_Type;
                        Data     : in out String;
                        Position : in out Integer)
   is
      N   : Node_Type;
      Pos : Index_Type := Start;
   begin

      loop
         N := Doc (Pos);
         Put (N.Data (1 .. Natural (N.Length)), Data, Position);
         if Position < 0
         then
            return;
         end if;
         exit when N.Next = Invalid_Relative_Index;
         Pos := Add (Pos, N.Next);
      end loop;
   end Serialize;

   type Mode_Type is (Mode_Open, Mode_Close);

   type Traversal_Type is
   record
      Index : Index_Type;
      Mode  : Mode_Type;
   end record;

   ------------
   -- Handle --
   ------------

   procedure Handle (Doc      : Subtree_Type;
                     Current  : Index_Type;
                     Mode     : Mode_Type;
                     Data     : in out String;
                     Position : in out Natural);

   procedure Handle (Doc      : Subtree_Type;
                     Current  : Index_Type;
                     Mode     : Mode_Type;
                     Data     : in out String;
                     Position : in out Natural)
   is
      Attr  : Index_Type;
      Value : Index_Type;
      Pos   : Relative_Index_Type;
      N     : constant Node_Type := Doc (Current);
   begin

      if N.Kind = Kind_Content and
         Mode = Mode_Open
      then
         Put_Escaped (Doc, Current, Data, Position);
         return;
      end if;

      if Mode = Mode_Close and N.Children /= Invalid_Relative_Index
      then
         Put ("</", Data, Position);
         Serialize (Doc, Current, Data, Position);
         Put (">", Data, Position);
      end if;

      if Mode = Mode_Open
      then
         Put ("<", Data, Position);
         Serialize (Doc, Current, Data, Position);
         Pos  := N.Attributes;
         Attr := Current;
         while Pos /= Invalid_Relative_Index
         loop
            Attr  := Add (Attr, Pos);
            Value := Add (Attr, Doc (Attr).Value);
            Put (" ", Data, Position);
            Serialize (Doc, Attr, Data, Position);
            Put ("=""", Data, Position);
            Serialize (Doc, Value, Data, Position);
            Put ("""", Data, Position);
            Pos := Doc (Attr).Next_Attribute;
         end loop;
         if N.Children = Invalid_Relative_Index
         then
            Put ("/", Data, Position);
         end if;
         Put (">", Data, Position);
      end if;
   end Handle;

   package S   is new SXML.Stack (Traversal_Type, 10000000);
   package Rev is new SXML.Stack (Index_Type, 20000000);

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Doc  : Subtree_Type;
                        Data : out String;
                        Last : out Natural)
   is
      Child    : Relative_Index_Type;
      Element  : Index_Type;
      Current  : Traversal_Type;
      Position : Integer := 0;
   begin
      Last := 0;
      S.Reset;
      Rev.Reset;
      S.Push ((Doc'First, Mode_Open));

      while not S.Is_Empty
      loop
         S.Pop (Current);
         Handle (Doc, Current.Index, Current.Mode, Data, Position);
         if Position < 0
         then
            return;
         end if;

         if Current.Mode = Mode_Open
         then
            if S.Is_Full
            then
               return;
            end if;
            S.Push ((Current.Index, Mode_Close));
            Element := Current.Index;
            Child   := Doc (Element).Children;
            loop
               exit when Child = Invalid_Relative_Index;
               Element := Add (Element, Child);
               if Rev.Is_Full
               then
                  return;
               end if;
               Rev.Push (Element);
               Child := Doc (Element).Siblings;
            end loop;
            while not Rev.Is_Empty
            loop
               Rev.Pop (Element);
               if S.Is_Full
               then
                  return;
               end if;
               S.Push ((Element, Mode_Open));
            end loop;
         end if;

      end loop;

      Last := Position;
   end To_String;

   overriding
   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is
      pragma Unreferenced (Left, Right);
   begin
      return Null_Tree;
   end "&";

end SXML;
