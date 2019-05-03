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

   type Attributes_Type (<>) is private;
   Null_Attributes : constant Attributes_Type;

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (Attributes : Attributes_Type) return Offset_Type
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
   with Ghost;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Document_Type) return Document_Type
   with
      Pre  => Is_Valid (Left, Right),
      Post => Num_Elements ("+"'Result) = Num_Elements (Left) + Num_Elements (Right);
   --  Concatenate documents
   --
   --  @param Left  First document
   --  @param Right Second document

   function "+" (Left, Right : Attributes_Type) return Attributes_Type
   with
      Pre  => Is_Valid (Left, Right),
      Post => Num_Elements ("+"'Result) = Num_Elements (Left) + Num_Elements (Right);
   --  Concatenate attributes
   --
   --  @param Left  First attributes
   --  @param Right Second attributes

   -------
   -- E --
   -------

   function E (Name       : Content_Type;
               Attributes : Attributes_Type;
               Children   : Document_Type) return Document_Type
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
      Pre      => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Null_Attributes) - Num_Elements (Children),
      Post     => E'Result /= Null_Document and
                  Num_Elements (E'Result) =
                     Num_Elements (Name) + Num_Elements (Null_Attributes) + Num_Elements (Children),
      Annotate => (GNATprove, Terminating);
   --  Construct element with child document and without attributes
   --
   --  @param Name        Name of element
   --  @param Children    Child documents

   function E (Name       : Content_Type;
               Attributes : Attributes_Type) return Document_Type
   is (E (Name, Attributes, Null_Document))
   with
      Pre      => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Attributes) - Num_Elements (Null_Document),
      Post     => E'Result /= Null_Document and
                  Num_Elements (E'Result) = Num_Elements (Name) +
                                            Num_Elements (Attributes) +
                                            Num_Elements (Null_Document),
      Annotate => (GNATprove, Terminating);
   --  Construct element with attributes and without child document
   --
   --  @param Name        Name of element
   --  @param Attributes  Attribute data

   function E (Name : Content_Type) return Document_Type
   is (E (Name, Null_Attributes, Null_Document))
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Null_Attributes) - Num_Elements (Null_Document),
      Post => E'Result /= Null_Document and
      Num_Elements (E'Result) = Num_Elements (Name) +
                                Num_Elements (Null_Attributes) +
                                Num_Elements (Null_Document),
      Annotate => (GNATprove, Terminating);
   --  Construct element without attributes and without child document
   --
   --  @param Name  Name of element

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

   type Attributes_Type is new Document_Type;
   Null_Attributes : constant Attributes_Type := Attributes_Type (Null_Document);

   --------------
   -- Is_Valid --
   --------------

   overriding
   function Is_Valid (Left  : Attributes_Type;
                      Right : Attributes_Type) return Boolean
   is (Is_Valid (Document_Type (Left), Document_Type (Right)));

end SXML.Generator;
