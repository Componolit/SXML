package SXML.Generator.Tests is

   pragma Assert (Num_Elements (Null_Tree) = 0);

   --  Simple element
   T001 : constant Subtree_Type := E ("elem1");

   --  Multiple elements
   T002 : constant Subtree_Type := E ("elem1") + E ("elem2");
   T003 : constant Subtree_Type := E ("elem1") + E ("elem2") + E ("elem3");

   --  Element with children
   T004 : constant Subtree_Type := E ("elem1", E ("child1"));
   T005 : constant Subtree_Type := E ("elem1", E ("child1") + E ("child2"));
   T006 : constant Subtree_Type := E ("elem1", E ("child1") + E ("child2") + E ("child3"));

   --  Element with attributes
   T007 : constant Subtree_Type := E ("elem1", A ("attr1", "value1"));
   T008 : constant Subtree_Type := E ("elem1", A ("attr1", "value1") +
                                               A ("attr2", "value2"));
   T009 : constant Subtree_Type := E ("elem1", A ("attr1", "value1") +
                                               A ("attr2", "value2") +
                                               A ("attr3", "value3"));

   --  Element with children and attributes
   T010 : constant Subtree_Type := E ("elem1", A ("attr1", "value1"), E ("child1"));
   T011 : constant Subtree_Type := E ("elem1", A ("attr1", "value1") + A ("attr2", "value2"),
                                               E ("child1") + E ("child2"));

end SXML.Generator.Tests;
