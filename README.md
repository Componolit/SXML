# SXML: A proof-of-concept XML generator in SPARK.

SXML is a PoC for generating XML in pure [SPARK 2014](http://spark-2014.org). The goal is to allow for a concise definition of XML documents directly inside the code, comparable to [Genodes XML generator](https://github.com/genodelabs/genode/tree/master/repos/os/src/test/xml_generator).

## Example

XML document to be generated:

```XML
<config>
  <report delay_ms=" 500"></report>
  <parent-provides>
    <service name="CAP"></service>
    <service name="CPU"></service>
  </parent-provides>
  <start name="myprog" caps=" 500">
    <binary name="myprog"></binary>
    <resource name="RAM" quantum="16MB"></resource>
    <config>
      <argv progname="myprog"></argv>
      <vfs>
        <dir name="dev">
          <log></log>
          <rtc></rtc>
          <null></null>
        </dir>
        <fs></fs>
      </vfs>
      <libc stdout="/dev/log" stderr="/dev/log" rtc="/dev/rtc">
      </libc>
    </config>
    <route>
      <any-service>
        <parent></parent>
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
        E ("route",
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
