with SXML.Generator; use SXML.Generator;
with SXML.Serialize; use SXML.Serialize;
with SXML.Debug;     use SXML.Debug;
with Ada.Text_IO;    use Ada.Text_IO;

use SXML;

procedure Simple
is
   -- <config>
   -- 	<report delay_ms="500"/>
   -- 	<parent-provides>
   -- 		<service name="CAP"/>
   -- 		<service name="CPU"/>
   -- 	</parent-provides>
   -- 	<start name="myprog" caps="500">
   -- 		<binary name="myprog"/>
   -- 		<resource name="RAM" quantum="16MB"/>
   -- 		<config>
   -- 			<argv progname="myprog"/>
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
   -- 		<route foo="54.1234>
   -- 			<any-service>
   -- 				<parent/>
   -- 			</any-service>
   -- 		</route>
   -- 	</start>
   -- </config>

   function Image (Val : Integer) return String is (Val'Img (Val'Img'First + 1 .. Val'Img'Last));

   Doc : Document_Type :=
   E ("config",
     E ("report", A ("delay_ms", Image (500))) +
     E ("parent-provides",
       E ("service", A ("name", "CAP")) +
       E ("service", A ("name", "CPU"))
     ) +
     E ("start", A ("name", "myprog") + A ("caps", Image (500)),
        E ("binary", A ("name", "myprog")) +
        E ("resource", A ("name", "RAM") + A ("quantum", "16MB")) +
        E ("config",
           E ("argv", A ("progname", "myprog")) +
           E ("vfs",
              E ("dir", A ("name", "dev"),
                 E ("log") +
                 E ("rtc") +
                 E ("null")) +
              E ("fs")
           ) +
           E ("libc", A ("stdout", "/dev/log") + A ("stderr", "/dev/log") + A ("rtc", "/dev/rtc"))
        ) +
        E ("route", A ("foo", "54.1234"),
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
   Debug.Dump (Doc);
   To_String (Doc, Data, Offset, Result);
   if Result = Result_OK
   then
      Put_Line (Data (1 .. Offset));
   else
      Put_Line ("Error: " & Result'Img);
   end if;
end Simple;
