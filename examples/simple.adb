with SXML.Generator;
use SXML;
use SXML.Generator;
with Ada.Text_IO; use Ada.Text_IO;

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

   Doc : Subtree_Type :=
   E ("config",
     E ("report", A ("delay_ms", 500)) +
     E ("parent-provides",
       E ("service", A ("name", "CAP")) +
       E ("service", A ("name", "CPU"))
     ) +
     E ("start", A ("name", "myprog") + A ("caps", 500),
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
        E ("route", A ("foo", 54.1234),
           E ("any-service",
              E ("parent")
           )
        )
     )
   );

begin
   Put_Line (To_String (Doc));
end Simple;
