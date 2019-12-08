# SXML: A formally verified XML library in SPARK.

[![Build Status](https://travis-ci.org/Componolit/SXML.svg?branch=master)](https://travis-ci.org/Componolit/SXML)

SXML is an XML library implemented in pure
[SPARK 2014](https://www.adacore.com/about-spark). Absence of runtime errors
and bounded stack usage have been proven for the library. This makes it a ideal
choice for processing information of untrusted origin.

The full API documentation is available in [doc/api/index.html](doc/api/index.html).

## Structure

The library consists of four parts: the generator, the parser, the serializer
and the query interface.

### SXML.Generator

The generator interface allows for declaring XML documents directly inside
SPARK code. This is much more concise and safe than constructing an XML
document by consecutive calls to API functions.

Constructors for elements (`E`), for attributes (`A`) and for content (`C`) can
be combined using the combination operator (`+`). The following XML document is
to be declared using the SXML generator:

```XML
<config>
  <report delay_ms="500"/>
  <parent-provides>
    <service name="CAP"/>
    <service name="CPU">Some content</service>
  </parent-provides>
</config>
```

The declaration is analogous to the XML document:

```Ada
with SXML.Generator;

procedure Simple
is
   use SXML.Generator;
begin
   Document : Document_Type :=
   E ("config",
     E ("report", A ("delay_ms", 500)) +
     E ("parent-provides",
       E ("service", A ("name", "CAP")) +
       E ("service", A ("name", "CPU"), C ("Some content"))
     )
   );

end Simple;
```

Refer to the sections on serialization and querying on how to use the resulting
document.

### SXML.Parser

This interface parses an XML document from its textual representations.
Parsing documents is very simple:

```Ada
with SXML.Parser;

procedure Parse
is
   use SXML.Parser;
   Input : String :=
     "<config>"
   & "   <report delay_ms=""500""/>"
   & "   <parent-provides>"
   & "      <service name=""CAP""/>"
   & "      <service name=""CPU"">Some content</service>"
   & "   </parent-provides>"
   & "</config>";
   Document : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);
   Result   : Match_Type;
   Position : Natural;
begin
   Parse (Data         => Input,
          Document     => Document,
          Parse_Result => Result,
          Position     => Position);
   if Result /= Match_OK
   then
      null;
   end if;
end Parse;
```

### SXML.Query

The query interface operates on a document (`SXML.Document_Type`) that was
parsed or constructed. There is a low level API that works on a state object of
type `SXML.Query.State_Type`. Before using the API, the state is initialized
for the document to be queried using the `Init` function. There are a number of
operations to navigate through the document and return data:

| **Operation**  | **Description**                         |
|:---------------|:----------------------------------------|
| Name           | Return name for current node            |
| Child          | Get child node of current node          |
| Sibling        | Get sibling node of current node        |
| Find_Sibling   | Find sibling of current node by name    |
| Attribute      | Get first attribute of an opening node  |
| Value          | Return value of current attribute       |
| Next_Attribute | Get next attribute of current attribute |
| Find_Attribute | Find attribute of current node by name  |

All operations have a result output parameter of type `Result_Type` indicating
whether the operation was successful, the data was not found (e.g. signaling
the last attribute of an element) or that an error occurred.

A more convenient way to obtain an element inside a document is the `Path`
operation. It receives a simple path as a string argument pointing to an
element starting from the root of the document, e.g. `/root/child/grandchild`.
As of now, exactly one element can be referenced in a path query. Attributes
can subsequently be queried using the `Find_Attribute` operation.

### SXML.Serialize

The serialization operation `To_String` converts a document into it's string
representation. The result is stored into a fixed buffer which must be large
enough to hold the result.

# Validation and Verification

A number of measures have been adopted to ensure that SXML correctly handles
XML files and that it does not crash when exposed to malicious data. We prove
the absence of runtime errors using `gnatprove` and bounded stack usage with
`gnatstack` (except for SXML.Generator which is inherently unbounded). We
fuzzed the parser and serializer using American Fuzzy Lop (AFL) for more than
200 million executions without any crash or hang. A test suite with more than
3000 tests is available to test conformance of the parser.

## Absence of Runtime Errors

To prove and build the library just type `make` in the root of the source
directory. The GNAT and SPARK toolset (Pro 20 or Community 2019) must be
installed and in your path.

## Bounded Stack Usage

To show stack usage run `make stack` in the root of the source directory. The
`gnatstack` tool (part of GNAT Pro) must be installed.

## Fuzzing

To fuzz the parser using AFL with the included `fuzzdriver` program, run `make
fuzz` in the root of the source directory. AFL must be installed.

## Tests

To run the unit tests only, type `make testonly` in the root of the source
directory. For the slightly bigger test suite including parser tests, type
`make testbulk` and for the full test suite run `make testinsane` (this will
download many large files from the Internet).

# Limitations

The largest name that can be found using `SXML.Query.Find_Attribute` and
`SXML.Query.Find_Sibling` is currently limited to 1024 characters due to an
internal fixed-size buffer that is used.

Special XML sections like CDATA, comments, DOCTYPE and procession information
are accepted by the parser, but ignored by all other parts of SXML.

As the constructor and combinator functions in `SXML.Generator` return
unbounded arrays of type `Document_Type` and `Attributes_Type`, those functions
are not bounded in stack usage. When using the generator interface, make sure
to use static inputs and manually review stack usage to prevent stack overflows.

# Authors and License

Adrian-Ken Rueegsegger (@Kensan), Alexander Senier (@senier)

This code is distributed under the terms of the GNU Affero General Public
License version 3, see LICENSE for details. Send email to sxml@componolit.com
for commercial licensing and support.
