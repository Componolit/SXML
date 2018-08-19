# SXML: XML generator in SPARK.

[![Build Status](https://travis-ci.org/Componolit/SXML.svg?branch=master)](https://travis-ci.org/Componolit/SXML)

SXML is a library for generating XML in pure [SPARK 2014](http://spark-2014.org). The goal is to allow for a concise definition of XML documents directly inside the code, comparable to [Genodes XML generator](https://github.com/genodelabs/genode/tree/master/repos/os/src/test/xml_generator). Dynamic generation of (parts of) the XML document is supported. Absence of runtime errors was proven for the library.

## Example

XML document to be generated:

```XML
<config>
  <report delay_ms="500"/>
  <parent-provides>
    <service name="CAP"/>
    <service name="CPU"/>
  </parent-provides>
  <start name="myprog" caps="500">
    <binary name="myprog"/>
    <resource name="RAM" quantum="16MB"/>
    <config>
      <argv progname="myprog"/>
      <vfs>
        <dir name="dev">
          <log/>
          <rtc/>
          <null/>
        </dir>
        <fs/>
      </vfs>
      <libc stdout="/dev/log" stderr="/dev/log" rtc="/dev/rtc"/>
    </config>
    <route foo="5.41234E+01">
      <any-service>
        <parent/>
      </any-service>
    </route>
  </start>
</config>
```

SPARK code to produce the XML:

```Ada
with SXML; use SXML;
with Ada.Text_IO; use Ada.Text_IO;

procedure Simple
is
   Doc : Subtree_Type :=
   E ("config",
     E ("report", A ("delay_ms", 500)) &
     E ("parent-provides",
       E ("service", A ("name", "CAP")) &
       E ("service", A ("name", "CPU"))
     ) &
     E ("start", A ("name", "myprog") & A ("caps", 500) &
        E ("binary", A ("name", "myprog")) &
        E ("resource", A ("name", "RAM") & A ("quantum", "16MB")) &
        E ("config",
           E ("argv", A ("progname", "myprog")) &
           E ("vfs",
              E ("dir", A ("name", "dev") &
                 E ("log") &
                 E ("rtc") &
                 E ("null")) &
              E ("fs")
           ) &
           E ("libc", A ("stdout", "/dev/log") & A ("stderr", "/dev/log") & A ("rtc", "/dev/rtc")
           )
        ) &
        E ("route", A ("foo", 54.1234) &
           E ("any-service",
              E ("parent")
           )
        )
     )
   );

begin
   Put_Line (To_String (Doc));
end Simple;
```

# Authors and License

Adrian-Ken Rueegsegger (@Kensan), Alexander Senier (@senier)

This code is distributed under the terms of the GNU Affero General Public License version 3, see LICENSE for details.
