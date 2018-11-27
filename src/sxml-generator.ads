--
--  @summary XML generator specification
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package SXML.Generator
is
   pragma Annotate (GNATprove, Terminating, SXML.Generator);

   type Attributes_Base_Type (<>) is private;
   Null_Attributes : constant Attributes_Base_Type;

   subtype Attributes_Type is Attributes_Base_Type
   with
      Dynamic_Predicate => Attributes_Type'First > 0 and Attributes_Type'Length > 0;

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (Attributes : Attributes_Base_Type) return Offset_Type
   with
      Annotate => (GNATprove, Terminating);
   --  Number of elements required for attributes
   --
   --  @param Attributes  Attributes

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Left  : Attributes_Type;
                      Right : Attributes_Type) return Boolean
   is ((Num_Elements (Left) > 0 or Num_Elements (Right) > 0) and
       Num_Elements (Left) <= Offset_Type (Index_Type'Last) - Num_Elements (Right))
   with Ghost;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Float) return Content_Type
   with
      SPARK_Mode,
      Post     => To_String'Result'Length < 12,
      Annotate => (GNATprove, Terminating);
   --  Convert float number to content
   --
   --  @param Value  Float number to convert

   function To_String (Value : Integer) return Content_Type
   with
      SPARK_Mode,
      Post     => To_String'Result'Length < 12,
      Annotate => (GNATprove, Terminating);
   --  Convert integer number to content
   --
   --  @param Value  Integer number to convert

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Document_Type) return Document_Type
   with
      Pre  => Is_Valid (Left, Right),
      Post => Num_Elements ("+"'Result) = Num_Elements (Left) + Num_Elements (Right),
      Annotate => (GNATprove, Terminating);
   --  Concatenate documents
   --
   --  @param Left  First document
   --  @param Right Second document

   function "+" (Left, Right : Attributes_Type) return Attributes_Type
   with
      Pre  => Is_Valid (Left, Right),
      Post => Num_Elements ("+"'Result) = Num_Elements (Left) + Num_Elements (Right),
      Annotate => (GNATprove, Terminating);
   --  Concatenate attributes
   --
   --  @param Left  First attributes
   --  @param Right Second attributes

   -------
   -- E --
   -------

   function E (Name       : Content_Type;
               Attributes : Attributes_Base_Type;
               Children   : Document_Base_Type) return Document_Type
   with
      Pre      => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Attributes) - Num_Elements (Children),
      Post     => E'Result /= Null_Document and
                  E'Result'Length = Num_Elements (Name) + Num_Elements (Attributes) + Num_Elements (Children),
      Annotate => (GNATprove, Terminating);
   --  Construct element with attributes and child document
   --
   --  @param Name        Name of element
   --  @param Attributes  Attribute data
   --  @param Children    Child documents

   function E (Name       : Content_Type;
               Children   : Document_Type) return Document_Type
   is (E (Name, Null_Attributes, Children))
   with
      Pre      => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Children),
      Post     => E'Result /= Null_Document and
                  Num_Elements (E'Result) =
                     Num_Elements (Name) + Num_Elements (Children),
      Annotate => (GNATprove, Terminating);
   --  Construct element with child document and without attributes
   --
   --  @param Name        Name of element
   --  @param Children    Child documents

   function E (Name       : Content_Type;
               Attributes : Attributes_Type) return Document_Type
   is (E (Name, Attributes, Null_Document))
   with
      Pre      => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Attributes),
      Post     => E'Result /= Null_Document and
                  Num_Elements (E'Result) = Num_Elements (Name) +
                                            Num_Elements (Attributes),
      Annotate => (GNATprove, Terminating);
   --  Construct element with attributes and without child document
   --
   --  @param Name        Name of element
   --  @param Attributes  Attribute data

   function E (Name : Content_Type) return Document_Type
   is (E (Name, Null_Attributes, Null_Document))
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last,
      Post => E'Result /= Null_Document and
      Num_Elements (E'Result) = Num_Elements (Name),
      Annotate => (GNATprove, Terminating);
   --  Construct element without attributes and without child document
   --
   --  @param Name  Name of element

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Integer) return Attributes_Type
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last - 2,
      Post => A'Result /= Null_Attributes and
              Num_Elements (A'Result) = Num_Elements (Name) + Num_Elements (To_String (Value)),
      Annotate => (GNATprove, Terminating);
   --  Construct attribute from integer number
   --
   --  @param Name  Name of attribute
   --  @param Value Integer number of attribute

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Float) return Attributes_Type
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last - 2,
      Post => A'Result /= Null_Attributes and
              Num_Elements (A'Result) = Num_Elements (Name) + Num_Elements (To_String (Value)),
      Annotate => (GNATprove, Terminating);
   --  Construct attribute from float number value
   --
   --  @param Name  Name of attribute
   --  @param Value Float number of attribute

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Content_Type) return Attributes_Type
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last and then
              Num_Elements (Value) <= Offset_Type (Index_Type'Last - Add (1, Num_Elements (Name))),
      Post => A'Result /= Null_Attributes and
              Num_Elements (A'Result) = Num_Elements (Name) + Num_Elements (Value),
      Annotate => (GNATproof, Terminating);
   --  Construct attribute from string value
   --
   --  @param Name  Name of attribute
   --  @param Value String value of attribute

   -------
   -- C --
   -------

   function C (Value : Content_Type) return Document_Type
   with
      Post => C'Result /= Null_Document and
              C'Result'Length = Num_Elements (Value),
      Annotate => (GNATprove, Terminating);
   --  Construct content value
   --
   --  @param Value String value of content

private

   type Attributes_Base_Type is new Document_Base_Type;
   Null_Attributes : constant Attributes_Base_Type := (1 .. 0 => Null_Node);

end SXML.Generator;
