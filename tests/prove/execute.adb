with SXML.Generator;
with SXML.Serialize;
with Ada.Text_IO;

use SXML;
use SXML.Generator;
use SXML.Serialize;

package body Execute
with
   SPARK_Mode,
   Refined_State => (Output => IO.State)
is
   package IO
   with
      Abstract_State => State,
      Initializes => State
   is
      procedure Put_Line (Data : String)
      with
         Global => (In_Out => State);
   end IO;

   package body IO
   with
      SPARK_Mode => Off
   is
      procedure Put_Line (Data : String) renames Ada.Text_IO.Put_Line;
   end IO;

   Dummy_Element : constant Document_Type := E ("arg", A ("value", String (Arg_Type'(others => 'x'))));
   Arg_Len       : constant Natural  := Natural (Num_Elements (Dummy_Element));

   pragma Assert (Arg_Len = 15);

   function To_Subtree (Arguments : Args_Type) return Document_Type
   with
     Pre  => Arguments'Length > 0 and Arguments'Length < 100,
     Post => Num_Elements (To_Subtree'Result) = Offset_Type (Arg_Len * Arguments'Length)
             and then Num_Elements (To_Subtree'Result) = Arguments'Length * 15;

   function To_Subtree (Arguments : Args_Type) return Document_Type
   is
      pragma Assert (Index_Type (Arg_Len) < Index_Type'Last / Arguments'Length);
      Result  : Document_Type (1 .. Index_Type (Arg_Len * Arguments'Length)) :=
         (others => Null_Node);
   begin
      for I in Relative_Index_Type range 0 .. Arguments'Length - 1
      loop
         declare
            Argument : constant Arg_Type := Arguments (Add (Arguments'First, I));
            Arg : constant Document_Type := E ("arg", A ("value", String (Argument)));
         begin
            if
              I in Relative_Index_Type'Last / Relative_Index_Type (Arg_Len)
              and then Relative_Index_Type (Arg_Len) * I <= Relative_Index_Type (Index_Type'Last - Result'First)
              and then Relative_Index_Type (Add (Result'First, Relative_Index_Type (Arg_Len) * I))
                       <= Relative_Index_Type (Index_Type'Last) - Relative_Index_Type (Arg_Len) - 1
              and then Add (Add (Result'First, Relative_Index_Type (Arg_Len) * I), Relative_Index_Type (Arg_Len) - 1)
                       <= Index_Type (Arg_Len * Arguments'Length)
            then
               Result (Add (Result'First, Relative_Index_Type (Arg_Len) * I) ..
                       Add (Add (Result'First, Relative_Index_Type (Arg_Len) * I),
                            Relative_Index_Type (Arg_Len) - 1)) := Arg;
            end if;
         end;
      end loop;
      pragma Assert (Arg_Len = 15);
      pragma Assert (Num_Elements (Result) = Offset_Type (Arguments'Length * Arg_Len));
      pragma Assert (Num_Elements (Result) = Arguments'Length * 15);
      return Result;
   end To_Subtree;

   function "+" (Value : String) return Arg_Type
   is
      Result : Arg_Type := (others => Character'Val (0));
   begin
      for I in 0 .. Value'Length - 1
      loop
         Result (Result'First + I) := Value (Value'First + I);
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

   procedure Execute (Program   : Content_Type;
                      Arguments : Args_Type)
   is
      Doc : Document_Type :=
       E ("config",
         E ("report",
           A ("delay_ms", "500")
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
            A ("caps", "500"),
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
      Offset : Natural;
      Result : Result_Type;
   begin
      To_String (Doc, Data, Offset, Result);
      if
        Result = Result_OK
        and Offset >= Data'First
        and Offset <= Data'Last
      then
         IO.Put_Line (Data (1 .. Offset));
      else
         IO.Put_Line ("Error:");
         IO.Put_Line (Result'Img);
      end if;
   end Execute;
end Execute;
