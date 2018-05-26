with Ada.Text_IO;
use Ada.Text_IO;

package body XML
   with SPARK_Mode
is

   type Kind_Type is (Kind_Invalid,
                      Kind_Element_Open,
                      Kind_Element_Close,
                      Kind_Attr_Integer,
                      Kind_Attr_Float,
                      Kind_Attr_String);

   type Name_Type is new String (1..100);
   Null_Name : constant Name_Type := (others => Character'Val (0));

   type Node_Type (Kind : Kind_Type := Kind_Invalid) is
   record
      Name : Name_Type;
      case Kind is
         when Kind_Invalid
            | Kind_Element_Open
            | Kind_Element_Close => null;
         when Kind_Attr_Integer =>
            Integer_Value : Integer;
         when Kind_Attr_Float =>
            Float_Value : Float;
         when Kind_Attr_String =>
            String_Value : Name_Type;
      end case;
   end record;
   Null_Node : constant Node_Type := (Kind => Kind_Invalid, Name => Null_Name);

   type Subtree_Type is array (Natural range <>) of Node_Type;
   Null_Tree : constant Subtree_Type (1..0) := (others => Null_Node);

   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is
      Len : constant Natural := Left'Length + Right'Length;
      Result : Subtree_Type (1 .. Len);
   begin
      Result (1 .. Left'Length) := Left;
      Result (Left'Length + 1 .. Result'Last) := Right;
      return Result;
   end "&";

   function To_Name (Name : String) return Name_Type
   is
      Result : Name_Type := (others => Character'Val (0));
   begin
      for I in Name'Range
      loop
         Result (I) := Name (I);
      end loop;
      return Result;
   end To_Name;

   function To_String (Name : Name_Type) return String
   is
      Len : Natural := 0;
   begin
      loop
         exit when Len >= Name'Last or else Name (Len + 1) = Character'Val (0);
         Len := Len + 1;
      end loop;
      return String (Name (1 .. Len));
   end To_String;

   function E (Name       : String;
               Children   : Subtree_Type := Null_Tree) return Subtree_Type
   is
      Index : Natural;
   begin
      return Result : Subtree_Type (1 .. Children'Length + 2)
      do
         Result (Result'First) := (Kind => Kind_Element_Open, Name => To_Name (Name));
         Index := 2;
         for Child of Children
         loop
            Result (Index) := Child;
            Index := Index + 1;
         end loop;
         Result (Result'Last) := (Kind => Kind_Element_Close, Name => To_Name (Name));
      end return;
   end E;

   function A (Name  : String;
               Value : Integer) return Subtree_Type
   is
   begin
      return (1 => (Kind          => Kind_Attr_Integer,
                    Name          => To_Name (Name),
                    Integer_Value => Value));
   end A;

   function A (Name  : String;
               Value : Float) return Subtree_Type
   is
   begin
      return (1 => (Kind        => Kind_Attr_Float,
                    Name        => To_Name (Name),
                    Float_Value => Value));
   end A;

   function A (Name  : String;
               Value : String) return Subtree_Type
   is
   begin
      return (1 => (Kind         => Kind_Attr_String,
                    Name         => To_Name (Name),
                    String_Value => To_Name (Value)));
   end A;

   procedure Print (Tree : Subtree_Type)
   is
      Is_Open : Boolean := False;
   begin
      for I in Tree'Range
      loop
         case Tree (I).Kind
         is
            when Kind_Element_Open =>
               if Is_Open
               then
                  Is_Open := False;
                  Put (">");
               end if;
               Is_Open := True;
               Put ("<" & To_String (Tree (I).Name));
            when Kind_Element_Close =>
               if Is_Open
               then
                  Is_Open := False;
                  Put (">");
               end if;
               Put ("</" & To_String (Tree (I).Name) & ">");
            when Kind_Attr_Integer =>
               Put (" " & To_String (Tree (I).Name) & "=""" & Tree (I).Integer_Value'Img & """");
            when Kind_Attr_String =>
               Put (" " & To_String (Tree (I).Name) & "=""" & To_String (Tree (I).String_Value) & """");
            when Kind_Attr_Float =>
               Put (" " & To_String (Tree (I).Name) & "=""" & Tree (I).Float_Value'Img & """");
            when Kind_Invalid =>
               Put ("INVALID");
         end case;
      end loop;
   end Print;

   function To_Subtree (Arguments : Arguments_Type) return Subtree_Type
   is
      Result : Subtree_Type (1 .. 3 * Arguments'Length);
   begin
      for I in 0 .. Arguments'Length - 1
      loop
         Result (Result'First + 3*I .. Result'First + 3*I+2) := E ("arg", A ("value", String (Arguments (Arguments'First + I))));
      end loop;
      return Result;
   end To_Subtree;

   procedure Exec (Program   : String;
                   Arguments : Arguments_Type)
   is
      Tree : Subtree_Type :=
       E ("config",
         E ("report",
           A ("delay_ms", 500)) &
         E ("parent-provides",
           E ("service", A ("name", "CAP")) &
           E ("service", A ("name", "CPU")) &
           E ("service", A ("name", "LOG")) &
           E ("service", A ("name", "PD")) &
           E ("service", A ("name", "ROM")) &
           E ("service", A ("name", "File_system")) &
           E ("service", A ("name", "Timer")) &
           E ("service", A ("name", "Rtc")) &
           E ("service", A ("name", "Report"))) &
         E ("start",
            A ("name", Program) &
            A ("caps", 500) &
            E ("binary",
               A ("name", Program)) &
            E ("resource",
               A ("name", "RAM") &
               A ("quantum", "16MB")) &
            E ("config",
               E ("argv",
                  A ("progname", Program) &
                  To_Subtree (Arguments)) &
               E ("vfs",
                  E ("dir",
                     A ("name", "dev") &
                     E ("log") &
                     E ("rtc") &
                     E ("null")) &
                  E ("fs")) &
               E ("libc",
                  A ("stdout", "/dev/log") &
                  A ("stderr", "/dev/log") &
                  A ("rtc", "/dev/rtc"))) &
               E ("route",
                  E ("any-service",
                     E ("parent")))));
   begin
      Print (Tree);
   end Exec;

   function "+" (Value : String) return Argument_Type
   is
      Result : Argument_Type := (others => Character'Val (0));
   begin
      for I in Value'Range
      loop
         Result (I) := Value (I);
      end loop;
      return Result;
   end "+";

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
