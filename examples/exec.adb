with SXML; use SXML;
with Ada.Text_IO; use Ada.Text_IO;

procedure Exec
with
   SPARK_Mode
is
   type Argument_Type is new String (1..100);
   type Arguments_Type is array (Index_Type range <>) of Argument_Type;

   function To_Subtree (Arguments : Arguments_Type) return Subtree_Type
   is
      Result : Subtree_Type (1 .. 3 * Arguments'Length);
   begin
      for I in 0 .. Index_Type'(Arguments'Length) - 1
      loop
         Result (Result'First + 3 * I .. Result'First + 3 * I + 2) :=
            E ("arg", A ("value", String (Arguments (Arguments'First + I))));
      end loop;
      return Result;
   end To_Subtree;

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

   procedure Execute (Program   : String;
                      Arguments : Arguments_Type)
   is
      Doc : Subtree_Type :=
       E ("config",
         E ("report",
           A ("delay_ms", 500)
         ) &
         E ("parent-provides",
           E ("service", A ("name", "CAP")) &
           E ("service", A ("name", "CPU")) &
           E ("service", A ("name", "LOG")) &
           E ("service", A ("name", "PD")) &
           E ("service", A ("name", "ROM")) &
           E ("service", A ("name", "File_system")) &
           E ("service", A ("name", "Timer")) &
           E ("service", A ("name", "Rtc")) &
           E ("service", A ("name", "Report"))
         ) &
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
                  To_Subtree (Arguments)
               ) &
               E ("vfs",
                  E ("dir",
                     A ("name", "dev") &
                     E ("log") &
                     E ("rtc") &
                     E ("null")) &
                  E ("fs")
               ) &
               E ("libc",
                  A ("stdout", "/dev/log") &
                  A ("stderr", "/dev/log") &
                  A ("rtc", "/dev/rtc")
               )
            ) &
            E ("route",
               E ("any-service",
                  E ("parent")
               )
            )
         )
       );
   begin
      Put_Line (To_String (Doc));
   end Execute;

   A : constant Arguments_Type := (+"foo", +"bar", +"baz");
begin
   Execute ("foo", A);
end Exec;
