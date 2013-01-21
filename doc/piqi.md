Overview
--------

Piqi is a high-level data definition language for [Piq](/doc/piq/) and its
encodings.

Below is a brief overview of Piqi features.

-   Piqi supports a rich set of types.

    primitive types:
    :   boolean, integer, single- and double-precision floating point number,
        string (Unicode string), binary (byte array), piq-text (verbatim text)

    user defined types:
    :   record, variant ([tagged
        union](http://en.wikipedia.org/wiki/Tagged_union)), enum, list and
        alias.

-   Piqi has high-level modules.

    Piqi modules provide several mechanisms for reusing type definitions from
    other modules:

    imports
    :   A module can *import* another module. It allows to use types defined in
        the imported module in local type definitions.

    includes
    :   A module can *include* another module to reuse all of its type
        definitions, imports and extensions as if they where defined locally.

    Both 'include' and 'import' mechanisms rely on elaborate module naming
    scheme.

-   Piqi supports type extensions and data schema evolution.

    All Piqi types can be extended either directly or through *extensions*
    mechanism:

    -   more fields can be added to a `record` definition

    -   more options can be added to a `variant` or `enum` definition

    -   more properties can be assigned to an `alias` type

    Backward and forward schema compatibility can be maintained by using Google
    Protocol Buffers approach when a unique integer number is assigned to each
    field in a record (read more about it in Google Protocol Buffers
    documentation).

    If schema compatibility is not needed (e.g. during prototyping stage), Piqi
    can assign field numbers automatically. Piqi can also automatically assign
    `enum` values.

-   Piqi module *is* represented as Piq object.

    Piqi language is based on Piq syntax and Piqi module structure is described
    in a series of Piqi specifications.

    As a result, Piqi module can be turned into a Piq object by taking the
    contents of the `.piqi` file, where the module is defined, and wrapping them
    in `:piqi/piqi [ ... ]` container.

    One of the benefits of such representation is the ability to serialize Piqi
    modules in the same way as any other Piq data objects.

-   Piqi type definition language is extensible.

    One of interesting Piqi properties is that Piqi language implementation
    takes its own high-level specification, written in Piqi, and parses the
    language into AST (which also serves as intermediate representation) without
    any hand-written parsing rules -- it is all fully automated.

    This mechanism allows easy extension of Piqi language. When adding new
    features, there is no need to design new syntax elements, update parsing
    code and transform AST into intermediate language. Also, new extensions are
    typically transparent for the core Piqi implementation.

Piqi borrows many concepts from Google Protocol Buffers which at the moment is
much better documented than Piqi. It may be useful to get familiar with Protocol
Buffers along with reading Piqi documentation.

For those who are familiar with Google Protocol Buffers, information about its
compatibility with Piqi can be found on [this page](/doc/protobuf/).

Some examples of Piqi specifications can be found [here](/examples/). The most
complex Piqi specification example is the Piqi self-specification which is
available [here](/self-definition/).

Lexical conventions
-------------------

The Piqi language described in the remaining part of the document is based on
Piq syntax which is specified [here](/doc/piq/).

In addition to general Piq rules, Piqi relies on some extra syntax elements,
such as identifiers which are used for type names, field name, option names,
etc.

Piqi identifier has the following format:

    <identifier> ::= ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-']*

Piqi identifiers are case-sensitive.

*(Note, \`true\` and \`false\` are reserved for boolean literals -- they can not
be used as identifiers.)*

**NOTE:** use of underscore (`_`) characters in Piqi identifiers is not allowed.
Hyphens (`-`) should be used instead.

Modules
-------

Piqi modules are defined in non-empty files with `.piqi` extensions. Each
`.piqi` file represents one Piqi module.

Piqi modules converted from Google Protocol Buffer specification normally have
`.proto.piqi` file extension.

`.piqi` and `.proto.piqi` are the only file extensions allowed for Piqi modules.
Other extensions are not recognized by [Piqi tools](/doc/tools/). (When Piqi
implementations resolves Piq types or Piqi module names it searches for files
with `.piqi` or `.proto.piqi` extensions using module search paths.)

There are also several restrictions to `.piqi` file names since they are used as
a part of Piqi module name. See the next section for details.

Each Piqi module may contain the following entries:

-   module name

-   import directives

-   include directives

-   type definitions

-   type extension directives

### Module names

Module names can be *local* or *global*. Global names contain an Internet domain
name as their first component. Local names rely on module's location in the
local filesystem.

Modules with global names must explicitly define them using
`.module <module-name>` directive at a top-level element of the `.piqi` file.
For example:

    .module piqi.org/piqi

Modules with local names (i.e. that doesn't start with a domain name) must not
define module name inside the module specification.

Module names consist of two main parts: *module path* and *piqi file basename*.

*Module path* for local modules is a local filesystem path leading to a `.piqi`
file.

*Module path* for global modules looks like a full URL that start with a domain
name.

*Piqi file basename* is the name of a `.piqi` file with removed extensions.

Module names can be formally described using the following specification:

    <module-name> :: <global-module-name> | <local-module-name>

    <global-module-name> :: <domain-name> '/' <local-module-name>

    <local-module-name> ::= (<path> '/')? <piqi-file-basename>

    <domain-name> ::= lowercased Internet domain name as defined by RFC 1034

    <piqi-file-basename> ::= ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*

    <path> ::=   <path-element>
               | <path> '/' <path-element>

    <path-element ::= ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.']+

**Examples:**

    % global module names:
    piqi.org/piqi

    example.com/2.1.2-rc1/mod

    example.com/58d3d4c1d/mod

    % local module name (corresponds to foo/v1.2/mod.piqi file in the local
    % filesystem):
    foo/v1.2/mod

### Module imports

*Imports* provides a way to use types defined in other Piqi modules in local
type definitions.

In order to use other module's types, the module must be first "imported" using
`import` directive.

Import directive defines the following properties:

-   module name

    Name of the imported module.

    The last component of the imported module name -- *piqi file basename* --
    will be used as *import name* unless *import name* is specified explicitly.

-   import name

    When specified, it overrides the default import name derived from imported
    module's name.

Names for types from the imported module have the following form:
`<import-name>/<type-name>`. See "Record" section below for more details.

**Examples:**

    .import [
        .module example.com/foo
    ]

    % overriding implicitly derived import name "foo" with "bar"
    .import [
        .module example.com/foo
        .name bar
    ]

### Module includes

*Include* mechanism provide a way to reuse type definitions, imports, extensions
and other top-level entries from another module as if they where defined
locally.

A module can include several other modules to combine their contents together.

`include` directive is used to specify module inclusion. It has only one
property which the name of the included module.

**Example:**

    .include [
        .module example.com/foo
    ]

### Extension modules

Extension module is a Piqi module that has a second extension in its file name.
For example, "m.ocaml.piqi" is an extension module for a regular Piqi module
"m.piqi".

All operations applicable to regular Piqi modules are also supported for
extension modules. The difference is that extension modules can be included
automatically in the modules which they extend.

For instance, "piqic ocaml" and "piqic erlang" Piqi compilers try to
automatically include `<m>.ocaml.piqi` and `<m>.erlang.piqi` respectively for
each loaded module `<m>.piqi`.

Extension modules are useful for working with third-party Piqi or Protocol
Buffers definitions which, for example, may not define necessary OCaml- or
Erlang-specific properties in the first place.

Using this mechanism, it is possible to take any set of Piqi modules and write
custom extensions for them without modifying the original files. After that,
extensions can be loaded automatically for all recursively included and imported
Piqi modules.

Primitive types
---------------

`bool`
:   ...

`int`
:   `int` type represents signed integers. Supported range for this type is
    implementation- specific (i.e. depends on a certain Piqi mapping) but
    normally it should be capable for holding at least 32-bit signed integers.

    The maximum range for `int` type is the following:

    `(min(signed 64-bit integer), max(unsigned 64-bit integer))`.

    In addition to `int`, there are some other variations of integer types
    supported by default. They are defined as *aliases* of `int` type in Piqi
    self-definition:
    [piqi.piqi](http://github.com/alavrik/piqi/blob/dev/piqi/piqi.piqi). Each
    Piqi mapping should provide support for these types.

    Below is the full list of Piqi integer types. Their names reflect some
    properties associated with their binary encoding and language mappings. See
    for example, description for [Piqi--Protocol Buffers
    mapping](/doc/protobuf/).

    -   `int`

    -   `uint`

    -   `int32`

    -   `uint32`

    -   `int64`

    -   `uint64`

    -   `int32-fixed`

    -   `uint32-fixed`

    -   `int64-fixed`

    -   `uint64-fixed`

    -   `proto-int32`

    -   `proto-int64`

    If unsure which integer type to use, it is recommended to use `int`.

    `uint` can be a little bit more efficient compared to `int` when serialized
    to binary encoding.

    `int32`, `uint32`, `int64`, `uint64` are similar to `int` and `uint` but
    they guarantee the integer ranges associated with these types (32-bit and
    64-bit).

`float`, `float64`
:   IEEE 754 double-precision floating point.

`float32`
:   IEEE 754 single-precision floating point.

`string`
:   Unicode string.

`binary`
:   Byte array (sequence of bytes).

`piq-word`
:   Unicode string represented as *word literal* in Piq notation.

`piq-text`
:   Unicode string represented as *verbatim text literal* in Piq notation.

User-defined types
------------------

Each user-defined defines a new type name which must be unique within the
module's local namespace and must not override names of the Piqi built-in
primitive types (e.g. `int`, `float`, `string`).

The local namespace includes all names of Piqi *imports* and all names of
user-defined types.

Type name must be a valid `identifier`.

### Record

Record type is a container data type which specifies zero or more *fields* or
*flags*.

Fields define the following set of properties:

-   name

    Field name is represented as a valid Piqi `identifier`.

    Field name can be omitted. In such case it will be implicitly derived from
    the field's type name.

    Field names must be unique across all fields for a given record.

-   type name

    Field type name refer to one of the following:

    -   Built-in type. E.g. `int`, `string`, `bool`, etc.

    -   Another type defined within the local Piqi module.

    -   Type imported from another module. In this case *type name* will have
        the following format: `<import-name>/<type-name>`.

    Field that doesn't specify *type* is called *flag*. Since instance of such
    field doesn't have associated value, its presence in the record is
    meaningful by itself. Therefore the name -- *flag*.

    Flags must be defined with `optional` field mode.

-   mode

    Field mode be specified as `required`, `optional` or `repeated`. As follows
    from their names:

    -   `required` means that exactly one field instance must be present in the
        record. This is the default.

    -   `optional` field *may* be present in the record but not more than once.

    -   `repeated` means that *zero or more* instances of the field may be
        present in the record.

-   default value

    For `optional` fields it is possible to specify *default value* which can be
    used as a default field's value when the field is missing in the record
    representation. The actual use of default value depends on particular Piqi
    mapping.

    Default values are represented as Piq value of the field's type.

-   wire-packed flag for binary encoding

    Repeated fields of primitive numeric types (integers, floats, booleans and
    enums) can be represented in so called "packed" format. This format was
    introduced in Protocol Buffers 2.3.0 and provides a more compact encoding
    for repeated primitive types by basically omitting field tags for all fields
    but the first one and stacking fields values one after another. This is
    possible because primitive numeric types are self-delimited.

    In order to use such "packed" representation for a repeated field, specify
    `.wire-packed` as a field property.

**Examples:**

    % this is a record definition
    .record [
        .name r     % record name

        % required integer field
        .field [
            .name i
            .type int

            % NOTE: fields are "required" by default; one can specify it explicitly:
            %.required
        ]

        % optional string field
        .field [
            .name s
            .type string
            .optional
        ]

        % optional binary field with default value (NOTE: default values may only be
        % specified for optional fields)
        .field [
            .name b
            .type binary
            .optional
            .default "abc \xff\x00"
        ]

        % repeated floating point field
        .field [
            .name f
            .type float
            .repeated
        ]

        % this is a flag, its presence in the record is meaningful by itself, it
        % doesn't carry any additional value
        .field [
            .name flag

            % NOTE: flags must be defined as .optional; obviously, .default value
            % doesn't make any sense for flags and thus not allowed
            .optional
        ]

        % optional "self"
        .field [
            .name self
            .type r     % here referencing the record we're defining now
            .optional
        ]

        % another optional filed which references type defined below
        .field [
            % NOTE: if field name and type name are the same, field name may
            % be omitted
            .type v
            .optional
        ]

        % required field of type "t" imported from module "mod"
        .field [
            .type mod/t
        ]

        % repeated integer field represented using "packed" format in binary
        % encoding
        .field [
            .name p
            .type int
            .repeated
            .wire-packed
        ]
    ]

### Variant

*Variant* type, also known as [tagged
union](http://en.wikipedia.org/wiki/Tagged_union), specifies a set of *options*.
Only one *option* instance can form a variant value at a time.

For example, a well-known *enum* type is an example of variant type.

Options define *name* and *type name* properties in the same manner as fields
for the *record type*. The same rules and considerations apply for *option name*
and *option type name* as for *field name* and *field type name* (see above).

**Examples:**

    % definition of a variant
    .variant [
        .name v

        .option [
            .name i
            .type int
        ]

        .option [
            % NOTE: if option name and option type are the same, field name may
            % be omitted
            .type r
        ]

        .option [
            .type e
        ]

        % options may not be associated with any types, such options are similar to
        % those used in enums
        .option [
            .name flag
        ]

        % those used in enums
        .option [
            .name l
            .type v-list    % see below
        ]
    ]

### Enum

Enum type is a degenerated case of a variant type. Enum defines options
similarly to variant, but enum options don't have types.

**Examples:**

    .enum [
        .name e
        .option [ .name a ]
        .option [ .name b ]
        .option [ .name c ]
    ]

    .enum [
        .name months
        .option [ .name jan ]
        .option [ .name feb ]
        .option [ .name mar ] % ...
    ]

### List

*List type* represents a list of elements where all elements have the same type.

**Examples:**

    % list of v
    .list [
        % NOTE: "-list" suffix is not mandatory, however it is a style convention
        .name v-list
        .type v
    ]

    % list of built-in type
    .list [
        .name int-list
        .type int
    ]

    .list [
        .name int-list-list
        .type int-list
    ]

### Alias

*Alias* defines an alias for some other type which can be one of user-defined,
built-in or imported types.

**Examples:**

    % an alias
    .alias [
        .name a
        .type v
    ]

    % just to give an idea of how it can be used
    .alias [
        .name uuid
        .type binary
    ]

    .alias [
        .name epoch-seconds
        .type uint64
    ]

In Piqi aliases are also used to assign static properties for types. For
instance, all Piqi integer types other than `int` itself are defined as aliases
of the built-in `int` type. For example, this is the definition of `int64`:

    .alias [
        .name int64
        .type.int
        .wire-type.zigzag-varint    % type of binary (wire) encoding
        .proto-type "sint64"        % correspondent Protocol Buffers type
    ]

At the moment there aren't many properties implemented by Piqi, but the concept
itself is very powerful. For example, this is how custom formatting functions,
if implemented, could be applied using aliases:

    .alias [
        .name epoch-seconds
        .type uint64

        .format.date-time
    ]

    .alias [
        .name uuid
        .type binary

        .format.uuid
    ]

    .alias [
        .name sha1sum
        .type binary

        .format.sha1
    ]

Extensions
----------

Extensions mechanism allows to add more components and properties to Piqi
entries.

Extensions can be applied to user-defined types (including records, variants,
enums, lists and aliaes), fields, options, functions, function parameters and
imports.

Each extension has the following properties:

-   extension target

    Extension target specifies which Piqi entry to extend. It is a combination
    of entry type followed by distinct entry name.

    This is a full list of supported extension targets:

    -   `.typedef <type name>`

    -   `.field <record name>.<field-name>`

    -   `.option <variant or enum name>.<option-name>`

    -   `.import <import name>`

    -   `.function <function name>`

    Targets can be specified more than once. In such case, several entries will
    be extended at once using the same extension entries.

    Target must refer to a locally defined Piqi entry or entries included from
    other modules using `include` directive.

    Extensions of imported definitions are not supported.

    Extensions can not be applied to built-in types.

-   actual extension entry

    Extension entry specifies an object which will be added to the extended type
    definition as if it was defined there natively.

    For example, a *field* definition would be a typical extension entry for
    *record* type. Similarly, *option* entries would be typically used for
    extending *enum* or *variant* type.

For example, we can add a field to a previously defined record:

    .record [
        .name r
        .field [
            .name i
            .type int
        ]
    ]

    .extend [
        .typedef r
        .with.field [
            .name s
            .type string
        ]
    ]

Or we could use extension to add an enum clause:

    .enum [
        .name e
        .option [ a ]
    ]

    .extend [
        .typedef e
        .with.option [ .name b ]
    ]

Extending fields, options or function parameters requires a slightly different
target specification:

    .extend [
        .field r.i
        .with.erlang-name "erlang_i"
    ]

    .extend [
        .option e.b
        .with.ocaml-name "ocaml_b"
    ]

In the same manner we can add arbitrary properties to variants, lists, aliases,
functions and imports.

There is a good example that demonstrates the power of Piqi extensions. Piqi
implementation uses this mechanism to extend its own specification with extra
features. For example, the following specification extends Piqi to support
Protocol Buffers properties:

[piqi.proto.piqi](/self-definition/#piqi_proto_piqi)

Similarly, support for OCaml-specific Piqi properties is provided by these two
specifications:

[piqi.ocaml.piqi](http://github.com/alavrik/piqi/blob/dev/piqic/piqi.ocaml.piqi),
[piqi.ocaml-extensions.piqi](http://github.com/alavrik/piqi/blob/dev/piqic/piqi.ocaml-extensions.piqi)

Functions
---------

Piqi `function` directive provides a way to define abstract functions. Functions
were originally introduced for [Piqi-RPC](/doc/piqi-rpc/), which relies heavily
on high-level function definitions.

Each defined function has a name and 3 types of parameters: *input*, *output*
and *error*, all of which are optional.

The *error* parameter is a special type of output parameter. It is assumed that
when the function is called, only one of *output* or *error* parameters will be
returned.

It is possible for a function to not have any input or output parameters at all.
Such function represents a named synchronous call where the call is meaningful
by itself and no parameters are passed in any direction.

Input and output function parameters correspond to an arbitrary (primitive or
compound) named Piqi data types. That is a Piqi function takes a data structure
as the input parameter, and returns a data structure as the output parameter.

Examples:

    % function with no input and output parameters
    .function [
        .name foo
    ]

    % function with input, output and error parameters
    .function [
        .name foo

        .input int
        .output some-user-defined-type-name
        .error string
    ]

For extra convenience, function may define an input, output or error types
inline without having to define them separately:

    % function with a record input parameter and primitive output and error
    .function [
        .name bar

        .input [
            .field [
                .type int
                .optional
            ]
        ]
        .output int
        .error float
    ]

    % function with a record input that has a default value
    .function [
        .name baz

        .input [
            .field.record [
                .type int
                .optional
                .default 10
            ]
        ]
    ]

    % function with a variant input parameter
    .function [
        .name v

        .output.variant [
            .option [
                .name i
                .type int
            ]
            .option [
                .name f
                .type float
            ]
        ]
    ]

    % function with an enum error parameter
    .function [
        .name e

        .error.enum [
            .option [ .name a ]
            .option [ .name b ]
        ]
    ]

For each defined *input*, *output* or *error* parameter, a correspondent Piqi
alias or a compund type gets implicitly defined. Records, variants, lists and
enums are generated for inline parameter definitions, aliases are generated for
all other types that are referred by name.

For *input* parameters, the name of the alias or the compound type becomes
`<function-name>-input`. Similarly, names for *output* and *error* parameters
become `<function-name>-output` and `<function-name>-error` respectively.

Piqi-light syntax
-----------------

Piqi-light syntax is an experimental lightweight EBNF-like read-only notation
for Piqi type definitions. It provides a compact way of displaying type
definitions while omitting all non-significant properties that may be present in
the original Piqi specification.

Original Piqi syntax relies on Piq file format which is optimized for editing
convenience and extensibility. But the same properties that make Piqi/Piq such a
great format for editing and representing all the features, also make it
substantially verbose and uniform. Both verbosity and uniformity makes it harder
to consume Piqi for informational purposes. Piqi takes a lot of display space
and doesn't provide more prominent syntax for important properties such as names
and types which, in the absence of concrete syntax, get the same visual
treatment as other less important language properties.

On the other hand, in practice, type definitions are rarely modified once
initially written. Therefore it is feasible to have a type definition syntax
that is optimized for reading.

These considerations lead to the idea that maybe it is practical to have two
highly expressive syntaxes: one being optimized for reading, and another one --
for writing and extensibility.

After having both syntaxes implemented, benefits of such division are becoming
more obvious. It would be extremely hard to provide an efficient unified
language solution for both of these use-cases, especially considering how
different the current Piqi and Piqi-light notations are.

There is one very important feature that Piqi-light is missing at the moment. It
is hand-written comments from the original Piqi specification. Unfortunately, it
will remain this way until Piqi obtains a uniform method of writing
documentation sections that can be reliably represented and passed to
Piqi-light.

`.piqi` files can be printed in Piqi-light syntax using `piqi light`
[command](/doc/tools/#piqilight) (for the lack of a better name).

For examples of Piqi-light syntax visit [Examples](/examples/) and
[Self-definition](/self-definition/) pages. All `.piqi` files there have a tab
where they are displayed in Piqi-light syntax.

Style guidelines
----------------

### Names

Although Piqi doesn't enforce certain naming style, it is recommended to use
lowercase identifiers instead of "CamelCase" style.

This way they are more readable and will retain readability while being combined
with some future Piqi features.

Overall, high-level Piqi type definitions should resemble grammar rules.
Although the default Piqi syntax is fairly verbose, using lower-case identifiers
will leave the possibility to format Piqi definitions in a concise notation.

Piqi pretty-printer from [Piqi tools](/doc/tools/) can be used to convert
"CamelCase" identifiers to "camel-case" (`piqi pp --normalize <.piqi> file`
command).

### Names for `list` type

It is recommended to name `list` types by appending `-list` suffix to the
original type name.

For example, if we wanted to define a list of type `t`, we would name the type
`t-list`.

Using `*-list` names for non-list types should be avoided.

This is necessary for one of possible Piqi future features, where it would
recognize `*-list` type names as list types automatically removing the need for
defining `list` types manually.

### Code formatting

Since Piqi is based on Piq syntax, general [Piq](/doc/piq/) formatting rules
apply.

Notes
-----

-   For the sake of better readability and modularity Piqi doesn't support
    nested definitions.


