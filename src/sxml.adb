package body SXML
   with SPARK_Mode
is

   --------------
   -- Set_Data --
   --------------

   function Set_Data (Data : String;
                      Kind : Kind_Type) return Subtree_Type;

   function Set_Data (Data : String;
                      Kind : Kind_Type) return Subtree_Type
   is
      Num_Chunks : constant Index_Type :=
        (Data'Length - 1) / Data_Type'Length + 1;

      Data_Offset : Natural := 0;
      Chunk_Kind  : Kind_Type := Kind;
   begin
      return Result : Subtree_Type (1 .. Num_Chunks) := (others => Null_Node)
      do
         for Chunk of Result
         loop
            declare
               Chunk_Len : constant Natural :=
                 (if Data'Length - Data_Offset < Data_Type'Length
                  then Data'Length - Data_Offset
                  else Data_Type'Length);

               Elem          : Data_Type := (others => Character'Val (0));
               Data_Position : constant Natural := Data'First + Data_Offset;
            begin
               Elem (Data_Type'First .. Data_Type'First + Chunk_Len - 1) :=
                     Data (Data_Position .. Data_Position + Chunk_Len - 1);
               Chunk := Node_Type'(Kind   => Chunk_Kind,
                                   Length => Length_Type (Chunk_Len),
                                   Data   => Elem);
               Chunk_Kind := Kind_Data;
               Data_Offset := Data_Offset + Chunk_Len;
            end;
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

   ---------------
   -- Attr_Name --
   ---------------

   function Attr_Name (Name : String) return Subtree_Type
   is (Set_Data (Name, Kind_Attr_Name));

   ---------------
   -- Attr_Data --
   ---------------

   function Attr_Data (Data : String) return Subtree_Type
   is (Set_Data (Data, Kind_Attr_Data));

end SXML;
