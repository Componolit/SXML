--
--  @summary Common operations implementation
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package body SXML
is
   pragma Annotate (GNATprove, Terminating, SXML);

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

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Node : Node_Type) return Boolean is
     (Node.Kind = Kind_Invalid);

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Node : Node_Type) return Boolean is
     (Node.Kind = Kind_Element_Open);

   ------------------
   -- Num_Elements --
   ------------------

   function Length (D : Content_Type) return Offset_Type is
     ((Offset_Type (D'Length + (Data_Type'Length - 1))) / Offset_Type (Data_Type'Length));

   function Num_Elements (Document : Document_Base_Type) return Offset_Type is
     (Offset_Type (Document'Length));

   ---------------
   -- Same_Kind --
   ---------------

   function Same_Kind (Current : Node_Type;
                       Old     : Node_Type) return Boolean is
     (Current.Kind = Old.Kind)
     with
       Annotate => (GNATprove, Inline_For_Proof);

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Document : in out Document_Type;
                         Offset   :        Offset_Type;
                         Name     :        Content_Base_Type) with
     Pre  => Has_Space (Document, Offset, Name)
             and then Add (Document'First, Offset) <= Document'Last,
     Post => Same_Kind (Document (Add (Document'First, Offset)),
                        Document (Add (Document'First, Offset))'Old);

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Document : in out Document_Type;
                         Offset   :        Offset_Type;
                         Name     :        Content_Base_Type)
   is
      Position : Natural := 0;
      Len      : Natural;
      NE       : constant Offset_Type := Length (Name);
   begin
      if NE = 0
      then
         return;
      end if;

      for I in Index_Type range
        Add (Document'First, Offset) .. Add (Add (Document'First, Offset), NE) - 1
      loop
         pragma Loop_Invariant (Position <= Name'Length);
         if Name'Length - Position > Document (I).Data'Length then
            Len := Data_Type'Length;
            Document (I).Next := 1;
         else
            Len := Name'Length - Position;
         end if;
         Document (I).Data (1 .. Len) :=
           Name (Name'First + Position .. Name'First + Position + Len - 1);
         Document (I).Length := Length_Type (Len);
         Position := Position + Len;
      end loop;
   end Put_String;

   ----------
   -- Open --
   ----------

   procedure Open (Name     :        Content_Type;
                   Document : in out Document_Type;
                   Position : in out Index_Type;
                   Start    :    out Index_Type)
   is
   begin
      Start := Position;
      Document (Start) := Null_Open_Element;
      Document (Start + 1 .. Add (Start, Length (Name) - 1)) := (others => Null_Data_Element);
      Put_String (Document, Sub (Start, Document'First), Name);
      Position := Add (Position, Length (Name));
   end Open;

   ------------
   -- Append --
   ------------

   procedure Append (Left   : in out Document_Type;
                     Offset :        Offset_Type;
                     Right  :        Document_Type)
   is
      Off : constant Index_Type := Index_Type (Offset_Type (Left'First) + Offset);
      I   : Relative_Index_Type := 0;
      N   : Index_Type;
   begin
      Left (Off .. Off - 1 + Right'Length) := Right;

      --  Find last element
      loop
         pragma Loop_Variant (Increases => I);
         pragma Loop_Invariant (I <= Relative_Index_Type (Offset));

         if Overflow (Left'First, I) then
            return;
         end if;

         N :=  Add (Left'First, I);
         if
            (N - Left'First) > Left'Length
            or (not (N in Left'Range)
                or else (Left (N).Kind /= Kind_Content
                         and Left (N).Kind /= Kind_Element_Open))
         then
            return;
         end if;

         exit when Left (N).Siblings = Invalid_Relative_Index;

         if Left (N).Siblings > Relative_Index_Type (Offset) - I then
            return;
         end if;

         I := I + Left (N).Siblings;
      end loop;

      Left (N).Siblings := Relative_Index_Type (Offset) - I;
   end Append;

   -----------------
   -- Put_Content --
   -----------------

   procedure Put_Content (Document : in out Document_Type;
                          Offset   :        Offset_Type;
                          Value    :        Content_Type)
   is
      Start : constant Index_Type := Add (Document'First, Offset);
   begin
      Document (Start) := Null_Content_Element;
      Document (Start + 1 .. Add (Start, Length (Value) - 1)) := (others => Null_Data_Element);
      Put_String (Document, Offset, Value);
   end Put_Content;

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (Name     :        Content_Type;
                        Data     :        Content_Base_Type;
                        Offset   : in out Offset_Type;
                        Document : in out Document_Type)
   is
      Name_Elements    : constant Offset_Type := Length (Name);
      Attribute_Offset : constant Offset_Type := Name_Elements + Offset;
      Data_Elements    : constant Offset_Type := Length (Data);
      Start            : constant Index_Type  := Add (Document'First, Offset);
      Last_Element     : constant Index_Type  := Add (Add (Start, Name_Elements), Data_Elements) - 1;
   begin
      Document (Start)                     := Null_Attribute_Element;
      Document (Start + 1 .. Last_Element) := (others => Null_Data_Element);

      Put_String (Document, Offset, Name);
      Document (Start).Next  := (if Name_Elements > 1 then 1 else Invalid_Relative_Index);
      Document (Start).Value := Relative_Index_Type (Name_Elements);
      if Data_Elements > 0 then
         Put_String (Document, Attribute_Offset, Data);
      end if;
      Offset := Offset + Name_Elements + Data_Elements;
   end Attribute;

   ----------------
   -- Get_String --
   ----------------

   function String_Length (Document : Document_Type;
                           Offset   : Offset_Type) return Natural
   is
      Pos : Index_Type := Add (Document'First, Offset);
      N   : Node_Type  := Document (Pos);
      Len : Natural := 0;
   begin
      loop
         pragma Loop_Variant (Decreases => Document'Last - Pos);
         if Len > Natural'Last - Natural (N.Length) then
            return 0;
         end if;
         Len := Len + Natural (N.Length);
         exit when N.Next = Invalid_Relative_Index or N.Next >= Sub (Index_Type'Last, Pos);
         Pos := Add (Pos, N.Next);
         exit when not (Pos in Document'Range);
         N := Document (Pos);
      end loop;
      return (if Len <= Natural'Last - Chunk_Length then Len else 0);
   end String_Length;

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String (Document :     Document_Type;
                         Start    :     Offset_Type;
                         Result   : out Result_Type;
                         Data     : out Content_Type;
                         Last     : out Natural)
   is
      Len    : constant Natural := String_Length (Document, Start);
      Offset : Natural := 0;
      Pos    : Index_Type := Add (Document'First, Start);
      N      : Node_Type  := Document (Pos);

   begin
      Last   := 0;
      Result := Result_Invalid;

      for D of Data
      loop
         D := Character'Val (0);
      end loop;

      if Len > Data'Length then
         Result := Result_Overflow;
         return;
      end if;

      loop
         pragma Loop_Variant (Decreases => Document'Last - Pos);
         pragma Loop_Invariant (Data'First <= Natural (Index_Type'Last) - Offset);

         --  FIXME: We can assume this already by the way we caluculated Length.
         if
            Natural (N.Length) > Data'Length - Offset
            or Offset > Natural'Last - Data'First - Natural (N.Length)
         then
            return;
         end if;

         Data (Data'First + Offset .. Data'First + Offset + Natural (N.Length) - 1) :=
            N.Data (N.Data'First .. N.Data'First + Natural (N.Length) - 1);
         Offset := Offset + Natural (N.Length);

         exit when N.Next = Invalid_Relative_Index or N.Next >= Sub (Index_Type'Last, Pos);

         Pos := Add (Pos, N.Next);
         if Pos not in Document'Range then
            return;
         end if;
         N := Document (Pos);
      end loop;

      if
         Offset = 0
         or Offset > Data'Length
      then
         return;
      end if;

      Last   := Data'First + Offset - 1;
      Result := Result_OK;
   end Get_String;

end SXML;
