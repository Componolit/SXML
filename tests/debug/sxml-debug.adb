with Ada.Text_IO;

package body SXML.Debug
with
   SPARK_Mode => Off
is
   ------------
   -- Offset --
   ------------

   function Offset (Current : Index_Type;
                    Off     : Relative_Index_Type) return String;

   function Offset (Current : Index_Type;
                    Off     : Relative_Index_Type) return String
   is
      A : constant Index_Type := Add (Current, Off);
   begin
      if Off = Invalid_Relative_Index
      then
         return "- [0]";
      end if;
      return A'Img (A'Img'First + 1 .. A'Img'Last) & " [" & Off'Img & "]";
   end Offset;

   ----------
   -- Repr --
   ----------

   function Repr (I : Index_Type;
                  N : Node_Type) return String;

   function Repr (I : Index_Type;
                  N : Node_Type) return String
   is
   begin
      if N.Kind = Kind_Invalid
      then
         return "[Invalid]";
      end if;

      return N.Kind'Img
        & " (Length:" & N.Length'Img
        & ", Next: " & Offset (I, N.Next)
        & ", Data: """ & N.Data (N.Data'First .. N.Data'First + Natural (N.Length) - 1) & """"
        & (case N.Kind is
              when Kind_Element_Open =>
                ", Attributes: " & Offset (I, N.Attributes) &
                ", Children: " & Offset (I, N.Children) &
                ", Siblings: " & Offset (I, N.Siblings),
              when Kind_Invalid => "",
              when Kind_Content => "",
              when Kind_Data => "",
              when Kind_Attribute =>
                ", Next_Attribute: " & Offset (I, N.Next_Attribute) &
                ", Value: " & Offset (I, N.Value))
        & ")";
   end Repr;

   ----------
   -- Dump --
   ----------

   procedure Dump (Context : Document_Type;
                   Short   : Boolean := False;
                   Message : String  := "Dumping context")
   is
      use Ada.Text_IO;
   begin
      Put_Line (Message & ":");
      for I in Context'Range
      loop
         Put (I'Img (I'Img'First + 1 .. I'Img'Last) & ":");
         Put_Line ("   " & Repr (I, Context (I)));
         exit when Short and Context (I) = Null_Node;
      end loop;
   end Dump;

end SXML.Debug;
