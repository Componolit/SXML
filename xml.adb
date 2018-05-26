package body XML
   with SPARK_Mode
is

   type Kind_Type is (Kind_Invalid, Kind_Element);

   type Node_Type is
   record
      Kind : Kind_Type;
      Name : String (1..100);
   end record;
   Null_Node : constant Node_Type := (Kind => Kind_Invalid, Name => (others => Character'Val (0)));

   type Subtree_Type is array (Natural range <>) of Node_Type;
   Null_Tree : constant Subtree_Type (1..0) := (others => Null_Node);

   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is
      Len : constant Natural := Left'Length + Right'Length;
      Result : Subtree_Type (1 .. Len);
   begin
      Result (1 .. Left'Length - 1) := Left; 
      Result (Left'Length .. Result'Last) := Right; 
      return Result;
   end "&";

   function E (Name     : String;
               Children : Subtree_Type := Null_Tree) return Subtree_Type
   is
   begin
      return Null_Tree;
   end E;

   procedure Exec (Program   : String;
                   Arguments : Arguments_Type)
   is
      Tree : Subtree_Type :=
       E ("foo",
         E ("baz",
           E ("blub")
         ) &
         E ("bar")
       );
   begin
      null;
   end Exec;

   -- <config>
   -- 	<report delay_ms="500"/>
   -- 	<parent-provides>
   -- 		<service name="CAP"/>
   -- 		<service name="CPU"/>
   -- 		<service name="LOG"/>
   -- 		<service name="PD"/>
   -- 		<service name="ROM"/>
   -- 		<service name="File_system"/>
   -- 		<service name="Timer"/>
   -- 		<service name="Rtc"/>
   -- 		<service name="Report"/>
   -- 	</parent-provides>
   -- 	<start name="write(0)" caps="500">
   -- 		<binary name="write"/>
   -- 		<resource name="RAM" quantum="16MB"/>
   -- 		<config>
   -- 			<argv progname="write">
   -- 				<arg value="/tmp/test"/>
   -- 				<arg value="This value"/>
   -- 			</argv>
   -- 			<environ>
   -- 				<env name="ENVVAR" value="42"/>
   -- 			</environ>
   -- 			<vfs>
   -- 				<dir name="dev">
   -- 					<log/>
   -- 					<rtc/>
   -- 					<null/>
   -- 				</dir>
   -- 				<fs/>
   -- 			</vfs>
   -- 			<libc stdout="/dev/log" stderr="/dev/log" rtc="/dev/rtc"/>
   -- 		</config>
   -- 		<route>
   -- 			<any-service>
   -- 				<parent/>
   -- 			</any-service>
   -- 		</route>
   -- 	</start>
   -- </config>
   
end XML;
