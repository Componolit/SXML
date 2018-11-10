with SXML.Generator;
with SXML.Serialize;
with Ada.Text_IO; use Ada.Text_IO;

use SXML;
use SXML.Generator;
use SXML.Serialize;

procedure Exec
with
   SPARK_Mode
is
   type Arg_Type is new String (1..100);
   type Args_Type is array (Index_Type range <>) of Arg_Type;

   function To_Subtree (Arguments : Args_Type) return Document_Type
   is
      Dummy   : constant Arg_Type := (others => 'x');
      Arg_Len : constant Natural  := E ("arg", A ("value", String (Dummy)))'Length;
      Result  : Document_Type (1 .. Index_Type (Arg_Len * Arguments'Length));
   begin
      for I in Relative_Index_Type range 0 .. Arguments'Length - 1
      loop
         declare
            Argument : constant Arg_Type := Arguments (Add (Arguments'First, I));
            Arg : constant Document_Type := E ("arg", A ("value", String (Argument)));
         begin
            Result (Add (Result'First, Relative_Index_Type (Arg_Len) * I) ..
                    Add (Add (Result'First, Relative_Index_Type (Arg_Len) * I), Relative_Index_Type (Arg_Len) - 1)) := Arg;
         end;
      end loop;
      return Result;
   end To_Subtree;

   function "+" (Value : String) return Arg_Type
   is
      Result : Arg_Type := (others => Character'Val (0));
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

   procedure Execute (Program   : String;
                      Arguments : Args_Type)
   is
      pragma Assert (Num_Elements (Null_Tree) = 0);

      Doc : Document_Type :=
       E ("config",
         E ("report",
           A ("delay_ms", 500)
         ) +
         E ("parent-provides",
           E ("service", A ("name", "CAP")) +
           E ("service", A ("name", "CPU")) +
           E ("service", A ("name", "LOG")) +
           E ("service", A ("name", "PD")) +
           E ("service", A ("name", "ROM")) +
           E ("service", A ("name", "File_system")) +
           E ("service", A ("name", "Timer")) +
           E ("service", A ("name", "Rtc")) +
           E ("service", A ("name", "Report"))
         ) +
         E ("start",
            A ("name", Program) +
            A ("caps", 500),
            E ("binary",
               A ("name", Program)) +
            E ("resource",
               A ("name", "RAM") +
               A ("quantum", "16MB")) +
            E ("config",
               E ("argv",
                  A ("progname", Program),
                  To_Subtree (Arguments)
               ) +
               E ("vfs",
                  E ("dir",
                     A ("name", "dev"),
                     E ("log") +
                     E ("rtc") +
                     E ("null")) +
                  E ("fs")
               ) +
               E ("libc",
                  A ("stdout", "/dev/log") +
                  A ("stderr", "/dev/log") +
                  A ("rtc", "/dev/rtc")
               )
            ) +
            E ("route",
               E ("any-service",
                  E ("parent")
               )
            )
         )
       );
      Data   : String (1 .. 5000);
      Offset : Natural := 0;
      Result : Result_Type;
   begin
      To_String (Doc, Data, Offset, Result);
      if Result = Result_OK
      then
         Put_Line (Data (1 .. Offset));
      else
         Put_Line ("Error: " & Result'Img);
      end if;
   end Execute;

   A : constant Args_Type := (+"foo", +"bar", +"baz");
begin
   Execute ("foo", A);
end Exec;
