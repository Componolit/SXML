package body SXML.Stack
is
   pragma Annotate (GNATprove, Terminating, SXML.Stack);

   Index : Natural := S'First;

   -----------
   -- Level --
   -----------

   function Level return Natural
   is (Index);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid return Boolean
   is (Index >= S'First and Index <= S'Last);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty return Boolean
   is (Index = S'First);

   -------------
   -- Is_Full --
   -------------

   function Is_Full return Boolean
   is (Index >= S'Last);

   ----------
   -- Push --
   ----------

   procedure Push (E : Element_Type)
   is
   begin
      S (Index) := E;
      Index := Index + 1;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (E : out Element_Type)
   is
   begin
      Index := Index - 1;
      E := S (Index);
   end Pop;

   ----------
   -- Drop --
   ----------

   procedure Drop
   is
   begin
      Index := Index - 1;
   end Drop;

   -----------
   -- Reset --
   -----------

   procedure Reset
   is
   begin
      Index := S'First;
   end Reset;

   ----------
   -- Init --
   ----------

   procedure Init
   is
   begin
      S := (others => Null_Element);
      Index := S'First;
   end Init;

end SXML.Stack;
