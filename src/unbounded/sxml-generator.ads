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

   subtype Attributes_Type is Attributes_Base_Type with
     Dynamic_Predicate => Attributes_Type'Last < Index_Type'Last;

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (Attributes : Attributes_Base_Type) return Offset_Type with
     Annotate => (GNATprove, Inline_For_Proof);
   --  Number of elements required for attributes
   --
   --  @param Attributes  Attributes

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Left  : Attributes_Base_Type;
                      Right : Attributes_Base_Type) return Boolean with
     Ghost,
     Annotate => (GNATprove, Inline_For_Proof);

   ---------
   -- "+" --
   ---------

   function "+" (Left  : Document_Type;
                 Right : Document_Type) return Document_Type with
     Pre  => Is_Valid (Left, Right),
     Post => Num_Elements ("+"'Result) = Num_Elements (Left) + Num_Elements (Right);
   --  Concatenate documents
   --
   --  @param Left  First document
   --  @param Right Second document

   function "+" (Left  : Attributes_Type;
                 Right : Attributes_Type) return Attributes_Type with
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
               Children   : Document_Base_Type) return Document_Type with
     Pre  => Length (Name) < Offset_Type (Index_Type'Last) - Num_Elements (Attributes) - Num_Elements (Children),
     Post => Valid_Document (E'Result)
             and then E'Result'Length = Length (Name) + Num_Elements (Attributes) + Num_Elements (Children);
   --  Construct element with attributes and child document
   --
   --  @param Name        Name of element
   --  @param Attributes  Attribute data
   --  @param Children    Child documents

   function E (Name     : Content_Type;
               Children : Document_Base_Type) return Document_Type is
     (E (Name, Null_Attributes, Children))
     with
       Pre  => Length (Name) < Offset_Type (Index_Type'Last) - Num_Elements (Children),
       Post => Valid_Document (E'Result)
               and then Num_Elements (E'Result) = Length (Name) + Num_Elements (Children);
   --  Construct element with child document and without attributes
   --
   --  @param Name        Name of element
   --  @param Children    Child documents

   function E (Name       : Content_Type;
               Attributes : Attributes_Type) return Document_Type is
     (E (Name, Attributes, Null_Document))
     with
       Pre  => Length (Name) < Offset_Type (Index_Type'Last) - Num_Elements (Attributes),
       Post => Valid_Document (E'Result)
               and then Num_Elements (E'Result) = Length (Name) + Num_Elements (Attributes);
   --  Construct element with attributes and without child document
   --
   --  @param Name        Name of element
   --  @param Attributes  Attribute data

   function E (Name : Content_Type) return Document_Type is
     (E (Name, Null_Attributes, Null_Document))
     with
       Pre  => Length (Name) < Offset_Type (Index_Type'Last),
       Post => Valid_Document (E'Result) and then Num_Elements (E'Result) = Length (Name);
   --  Construct element without attributes and without child document
   --
   --  @param Name  Name of element

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Content_Type) return Attributes_Type with
     Pre  => Length (Name) < Offset_Type'Last
             and then Length (Value) <= Offset_Type (Index_Type'Last - Add (1, Length (Name))),
     Post => A'Result /= Null_Attributes
             and Num_Elements (A'Result) = Length (Name) + Length (Value);
   --  Construct attribute from string value
   --
   --  @param Name  Name of attribute
   --  @param Value String value of attribute

   -------
   -- C --
   -------

   function C (Value : Content_Type) return Document_Type with
     Post => C'Result /= Null_Document
             and C'Result'Length = Length (Value);
   --  Construct content value
   --
   --  @param Value String value of content

private

   type Attributes_Base_Type is new Document_Base_Type;
   Null_Attributes : constant Attributes_Base_Type := Attributes_Base_Type (Null_Document);

   --------------
   -- Is_Valid --
   --------------

   overriding
   function Is_Valid (Left  : Attributes_Base_Type;
                      Right : Attributes_Base_Type) return Boolean is
     (Is_Valid (Document_Base_Type (Left), Document_Base_Type (Right)));

   ------------------
   -- Num_Elements --
   ------------------

   overriding
   function Num_Elements (Attributes : Attributes_Base_Type) return Offset_Type is
     (Attributes'Length);

end SXML.Generator;