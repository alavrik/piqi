This document describes various encodings for Piq data and Piq streams except
for the Piq language itself, which is documented [here](/doc/piq/).

Data represented in Piq format can be converted to and from these encodings
using [Piq tools](/doc/tools/).

Programs written in various programming languages can read and write data in
these formats using Google Protocol Buffers (available for Python, Java, C++ and
other language) or native Piqi language mappings (available for
[OCaml](/doc/ocaml/) and [Erlang](/doc/erlang)).

In addition to the two binary encodings described below, Piq data can be
converted to and from JSON and XML which allows easier integration with programs
written in dynamic languages.

Protocol Buffers binary format (pb)
-----------------------------------

Since there is a mapping from Piqi type specification to Protocol Buffers types,
encoding rules for Piq values can be described in two stages:

1.  Mapping from Piqi data types to Protocol Buffer data types.

    It is described in [Piqi -- Protocol Buffers compatibility](/doc/protobuf/)
    section of the current documentation.

2.  Rules for serializing Protocol Buffer data objects into binary format.

    This part is described in Google Protocol Buffers documentation:
    [http://code.google.com/apis/protocolbuffers/docs/encoding.html](http://code.google.com/apis/protocolbuffers/docs/encoding.html)

-   This encoding can be used for serializing one Piq data object at a time. It
    is not suitable for encoding Piq streams containing more that one object.

-   Information about object type is not included with the object.

-   Not all Piq values can be serialized using this encoding. Its usage is
    restricted only to "compound" Piq objects -- those of `record`, `variant` or
    `list` type (or `alias` of one of these three types).

All "compound" Piq objects are mapped to Protocol Buffers `messages` that
contains zero or more fields. For example, in case of `variant` it is exactly
one field that represents one of the variant's options. A `message` for a Piq
`record` will contain as many fields as the original record. Piq `list` will be
mapped to a `message` with a single *repeated* field representing elements of
the list.

In terms of Protocol Buffers, `messages` are encoded without any leading headers
or tags. Using such method, `message` is actually represented as a sequence of
encoded fields.

Each field has a small header that contains field's unique `number` within a
message and a `wire type` which corresponds to low-level encoding method used
for encoding the field. By knowing field's `wire type` it is possible to
calculate the length of the field's encoded representation.

More information about it can be found in Protocol Buffers documentation.

JSON
----

For examples of how various Piq objects are represented in JSON, see the
[examples page](/examples/).

Primitive Piq values, such as integers, floats and strings are represented using
correspondent JSON literals. Floating point NaN, positive and negative
infinities are represented as `"NaN`, `"Infinity"`, `"-Infinity"` JSON strings.

The only supported JSON encoding is UTF-8.

Piq binaries are represented as JSON strings containing Base64-encoded binary
values.

Piq records and variants are represented using JSON associative arrays. Repeated
fields can represented as JSON arrays or, optionally, as a single field if only
one instance of the field is defined.

Optional Piq fields that are missing have `null` as a JSON value. Alternatively,
it is possible to remove them from JSON output entirely by using a special
configuration option.

Piq flags, i.e. optional fields without an associated value, have a JSON boolean
value set to `true` when the flag is present.

Piq lists are mapped to JSON arrays.

[JSON RFC](http://tools.ietf.org/html/rfc4627) states that "A JSON text is a
serialized object or array". In other words, primitive values such as integers,
strings or booleans do not make valid top-level JSON. Because of that, all
top-level Piq values of primitive types are wrapped into a special top-level
object:

    {"_", <value>}

For example:

    {"_", "foo"}

    {"_", 10}

    {"_", false}

XML
---

Support for XML format further extends Piqi's ability to interoperate with
various systems. For instance, it is possible to use existing XML tools such as
XPath, XSLT and XQuery for analyzing, querying and transforming Piq data.

The only supported XML encoding is UTF-8.

Floating point NaN, positive and negative infinities are represented as `NaN`,
`Infinity`, `-Infinity` text nodes.

Piq binaries are represented as XML text nodes containing Base64-encoded binary
values.

Whitespace in text XML nodes is significant.

XML attributes and namespaces are not allowed.

DTD and XML Schema(s) are not supported.

In theory, Piqi definitions can be converted to XML Schema .xsd definitions (but
not DTDs) as they are roughly equivalent and serve the same goal. However, it is
hard to foresee all the difficulties and corner cases without actually
implementing it.

Binary encoding for Piq stream format (`wire`)
----------------------------------------------

**NOTE: This encoding is experimental and will be revised in the future version
of Piqi. The main idea will remain the same, but encoding scheme will change.
For instance, each stream element will be prepended with its size encoded using
varint encoding. The new encoding scheme will allow easy reading and writing
.wire files using Google Protocol Buffers and from Protobuf-enabled tools such
as Google's Sawzall language.**

Binary encoding for Piq streams allows a lot more than the *single object
encoding* described in the previous section.

-   It can be used for encoding Piq streams containing zero or more objects of
    different types. For example, using this encoding, `.piq` files with
    multiple objects can be converted to a compact binary form.

-   It stores information about types of the encoded objects which allows to
    convert binary-encoded data back to Piq or other formats. Type information
    is stored in the form of fully-qualified Piqi type names.

-   Optionally, type information about data contained in the stream can fully
    embedded in the stream. It is achieved by including serialized Piqi modules
    with type definitions.

-   Any Piq value can be encoded using this encoding. Unlike *single object
    encoding*, it is not limited to records, variants and lists.

-   Piq streams and their `wire` representation have 1-to-1 mapping.

At a high level, a Piq object is a pair of `(<type-name>, <encoded-object>)`,
where `<type-name>` is a fully-qualified Piqi name of the object's type and
`encoded-object` is a Piq object (value) encoded using *common binary encoding*.

The actual stream encoding separates type names from encoded objects by
introducing two separate low-level stream entries: type names and encoded
objects. This method allows to associate one `<type-name>` entry with many
object entries. If a stream contains several objects of the same type, the name
of the type can be included in the stream only once in order to preserve space.
It also simplifies programs that write streams containing only one type of
objects.

Encoded wire streams can contain three kinds of entries.

-   Type names.

    Type name entry is represented as a pair of `(<type-code>, <type-name>)`
    where `<type-code>` is an odd integer \>= 1, and `<type-name>` is a string.

    Type name entry essentially associates `<type-code>` with `<type-name>`.

    Subsequent type name entries in the stream override previous entries with
    the same type code.

    Type name with code `1` has a special meaning. It represents *default type
    name*. In Piq stream, default type names are defined by
    `:piqtype <typename>` directive.

-   Encoded objects.

    Encoded object entry is represented as a pair of
    `(<type-code> + 1, <encoded-object>)`, where `<type-code>` is a code for Piq
    object's type defined previously in the stream by correspondent *type name*
    entry.

    Object of a default type have code 2. They correspond to *untyped objects*
    in Piq stream.

-   Embedded Piqi modules.

    Embedded Piqi modules correspond to `:piqi [ ... ]` entries in Piq streams.
    In `wire` format, they are represented as objects of type
    `piqi.org/piqi/piqi` with code 1073741823 (2\^29 - 1) which is the maximum
    possible code value.

For example, if we have two objects `obj1` and `obj2` with the same typename
`typename1`, they can be encoded in a stream as:

    (1, typename1)
    (2, obj1)
    (2, obj2)

or in a more excessive way:

    (1, typename1)
    (2, obj1)
    (1, typename1)
    (2, obj2)

or even like that:

    (1, typename1)
    (2, obj1)
    (3, typename1)
    (4, obj2)

The first encoding is more compact compared to the other two. The last encoding
would be typically used if `obj1` and `obj2` had different types.

After that `(<type-code>, <type-name>)` and `(<type-code>, <encoded-object>)`
pairs are encoded using *common binary encoding* for fields (i.e. Protocol
Buffers encoding), where `<type-code>` represents field number and both
`<type-name>` and `<encoded-object>` represent field value of correspondent
type.

`piq-json`
----------

**NOTE: this format is experimental and will be changing soon.**

The purpose of `piq-json` format is to allow 2-way conversion between Piq stream
and JSON stream while preserving type information and embedded Piqi
specifications.

The difference between `json` and `piq-json` is that `piq-json` includes
information about Piq types along with data object and, also, can contain
embedded Piqi modules. `json`, on the other hand, doesn't preserve type
information and can contain only Piq data objects represented in JSON format.

This way, `piq-json` is an extension of `json` format that allows the encoded
stream to be converted back to `piq` or other supported formats.

`piq-json` format extends `json` by wrapping each Piq stream object, represented
in JSON format, into a special top-level JSON object.

Each data object is wrapped in a JSON object that has the following format.

    {
      "_piqtype": "<fully-qualified-piqi-typename>",
      "_piqobj": <piq-object-represented-in-JSON>
    }

Each embedded Piqi module is wrapped as follows:

    {
      "_piqi": <piqi-module--represented-in-JSON>
    }
