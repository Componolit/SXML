--
--  @summary XML generator implementation
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package body SXML.Generator
is
   pragma Annotate (GNATprove, Terminating, SXML.Generator);

   ------------------
   -- Num_Elements --
   ------------------

   overriding
   function Num_Elements (Attributes : Attributes_Base_Type) return Offset_Type
   is (Attributes'Length);

   -------
   -- E --
   -------

   function E (Name       : Content_Type;
               Attributes : Attributes_Type;
               Children   : Document_Base_Type) return Document_Type
   is
      Len : constant Index_Type :=
         Index_Type (Num_Elements (Name) + Num_Elements (Attributes) + Num_Elements (Children));

      Result   : Document_Type (1 .. Len) := (others => Null_Node);
      Position : Index_Type := Result'First;
      Start    : Index_Type;
   begin
      Open (Name, Result, Position, Start);
      Result (Start).Attributes :=
         (if Attributes = Null_Attributes
          then Invalid_Relative_Index
          else Relative_Index_Type (Num_Elements (Name)));
      Result (Start).Children :=
         (if Children = Null_Document
          then Invalid_Relative_Index
          else Relative_Index_Type (Num_Elements (Name) + Num_Elements (Attributes)));

      if Attributes /= Null_Attributes
      then
         Result (Position .. Add (Position, Num_Elements (Attributes)) - 1) := Document_Type (Attributes);
      end if;

      if Children /= Null_Document
      then
         pragma Assert (Children'Length = Num_Elements (Children));
         Result (Add (Position, Num_Elements (Attributes)) ..
                 Add (Add (Position, Num_Elements (Attributes)), Num_Elements (Children)) - 1) := Children;
      end if;

      return Result;
   end E;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Content_Type) return Attributes_Type
   is
      Result : Document_Type (1 .. Sub (Add (Add (1, Num_Elements (Name)), Num_Elements (Value)), 1));
      Offset : Offset_Type := 0;
   begin
      Result := (others => Null_Node);
      SXML.Attribute (Name     => Name,
                      Data     => Value,
                      Offset   => Offset,
                      Document => Result);
      pragma Unreferenced (Offset);
      return Attributes_Type (Result);
   end A;

   -------
   -- C --
   -------

   function C (Value : Content_Type) return Document_Type
   is
      Result : Document_Type (1 .. Add (1, Num_Elements (Value) - 1)) :=
         (others => Null_Node);
   begin
      Put_Content (Result, 0, Value);
      return Result;
   end C;

   ---------
   -- "+"--
   ---------

   function "+" (Left, Right : Document_Type) return Document_Type
   is
      Result : Document_Type (1 .. Index_Type (Num_Elements (Left) + Num_Elements (Right))) := (others => Null_Node);
   begin
      if Right'Length = 0
      then
         return Left;
      end if;

      Result (1 .. Left'Length) := Left;
      Append (Result, Offset_Type (Left'Length), Right);
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Attributes_Type) return Attributes_Type
   is
      Result : Attributes_Type (1 .. Left'Length + Right'Length);
      I      : Relative_Index_Type := 0;
      N      : Index_Type;
   begin
      --  This yields a SPARK bug box (cf. RA06-002)
      --  Result := (Result'Range => Null_Node);

      Result (1 .. Left'Length) := Left;

      pragma Annotate (GNATprove, False_Positive, """Result"" might not be initialized",
                      "Aggregate initialization yields a bug box, cf. RA06 - 002");

      Result (Left'Length + 1 .. Left'Length + Right'Length) := Right;

      pragma Assert (Num_Elements (Result) = Num_Elements (Left) + Num_Elements (Right));

      --  Find last attibute
      loop
         if Overflow (Result'First, I)
         then
            return Result;
         end if;

         N :=  Add (Result'First, I);
         if I > Left'Length or
            (not (N in Result'Range) or else
             Result (N).Kind /= Kind_Attribute)
         then
            return Result;
         end if;

         pragma Loop_Variant (Increases => I);
         pragma Loop_Invariant (N in Result'Range);
         pragma Loop_Invariant (Result (N).Kind = Kind_Attribute);
         pragma Loop_Invariant (I <= Left'Length);

         exit when Result (N).Next_Attribute = Invalid_Relative_Index;

         if Result (N).Next_Attribute > Relative_Index_Type'Last - I
         then
            return Result;
         end if;

         I := I + Result (N).Next_Attribute;
      end loop;

      Result (N).Next_Attribute := Left'Length - I;
      return Result;
   end "+";

end SXML.Generator;
