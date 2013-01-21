Overview
--------

Piq is a language for representing structured data. It allows humans to
conveniently read, write and edit structured data. Piq has the following
features:

-   Minimalist and powerful syntax.

    -   No syntax noise compared to XML.

    -   Reasonable amount of parenthesis compared to S-expressions.

    -   Comments

-   Rich set of data literals, such as:

    -   Unicode strings, booleans, integer and floating point numbers (including
        "infinity" and "NaN");

    -   binary strings (byte arrays);

    -   verbatim text;

    -   lists.

Piq is designed to be used together with [Piqi data definition
language](/doc/piqi/). In fact, Piq is fairly useless without Piqi, since there
are no tools for processing it dynamically. There are some Piq features that
heavily rely on knowing data types. For example, in certain cases it is possible
to omit field names in record representation.

Primitive values such as integers, booleans and strings are first-class values
in Piq. Such treatment of data makes Piq closer to programming languages rather
than to XML or JSON that were designed for representing "documents" or
"structured objects".

Piq is a stream format: one Piq file (stream) can contain multiple typed data
objects.

Although Piq is a text language, the actual *data* represented in Piq files can
be converted to [binary or JSON encodings](/doc/encodings/) and the other way
around.

If you are not familiar with Piq and want to get basic feeling of what it looks
like you can click though some real [examples](/examples/) and/or skim through
the rest of the page.

After getting familiar with the basics, refer to [Piqi
documentation](/doc/piqi/) to get information about what data structures can be
represented in Piq.

At the moment there is no comprehensive reference manual, so a lot of concepts
below will be introduced and explained by example.

Lexical conventions
-------------------

Piq is a text-based language. Piq is represented as valid Unicode text encoded
in UTF-8. Other text encodings are not supported.

### Ignored whitespace

Whitespace is one or more of the following characters sequences:
`" ", "\t", "\n", "\r\n"`.

Whitespace serves as a token separator.

#### End of line ("EOL")

`"\n"` or `"\n\r"` sequences are recognized as *end of line*.

`"\r"` without following `"\n"` is not allowed.

### Comments

Comments is any sequence of characters beginning with `'%'` and continuing till
the end of the line, excluding cases in which `'%'` character occur in *string*
or *verbatim test* literals.

Currently comments are not recognized as tokens.

### Boolean literals

There are only two valid values for boolean literals (they are also the only
keywords in Piq):

    true

    false

### Number literals

#### Integer literals

**Examples:**

    % base 10 integers

    0 -1 100 1_000_000_000


    % conventional base 16 integers

    0xffff -0xffff_0000


    % base 2 integer literal

    0b1111_1111_1111_1111

Integer *literals* also represent valid floating point *values*.

#### Floating point literals

**Examples:**

    0.0 -10.0

    3.14159

    -2e15
    5.6e-10


    % not a number, positive and negative infinity

    0.nan 0.inf -0.inf

### String literals

**Examples:**

    "this is a string\n"

    % "hi" in Russian (utf-8 encoded Cyrillic characters)
    :string "привет"

    % these are supported string escape sequences
    "\" \t\n\r  \x20 \u0020 \U00000020"

    "\x74\x79\x70\x65"

    % NOTE: octal escape codes are unsupported (e.g. "\000\123")

    % binary literal must not contain Unicode characters with codes > 127, any string
    % literal containing ASCII-characters represents a valid binary value

    "this is a binary literal"

    "binary literals may contain bytes encoded like this: \xfe"

    "non-unicode escapes are also allowed in binary literals: \" \t\r\n"

String *literals* are used for representing both Unicode `string` *values* and
`binary` (i.e. byte array) *values*.

There are several simple rules that restrict usage of some string sequences
depending on which type this literal is mapped to.

-   ASCII strings represent valid values for both `string` and `binary` type.

-   Unicode characters and escape sequences for unicode characters (those
    starting with `'U'` or `'u'`) are prohibited in binaries.

-   Hex escape sequences (those starting with 'x') with codes greater than 127
    are prohibited in strings.

Note, that there is no character literal in Piq (and no character type in Piqi).

### Verbatim text literal

Verbatim text literal is represented by one line or several contiguous lines of
text that start with `'#'`, followed by a single space character, and continue
to the end of the line.

**Examples:**

    # single-line verbatim text

    # multi-line verbatim test
    # here's another line
    #
    # and another one
    #

    % leading whitespace is ignored:

            # one more line of text

        #
        # that's it

Verbatim text literals can also be used for representing string values.

### Word literal

Word is a contiguous sequence of characters, delimited by whitespace or one of
these special characters: `'(' ')' '[' ']' '{' '}' '"' '%' '#'`. Words also
can't contain non-printable characters like characters from the lower ASCII
range on non-printable Unicode characters.

NOTE: `true` and `false` words are recognized as boolean literals.

**Examples:**

    abc

    !!!!!

    +

    --

    *0-=+q`~@j\/&

### Name literal

Name is a *word* starting with `'.'` character. Remaining characters form an
`identifier` which is defined as follows.

    <identifier> ::= ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-']*

*(Note, \`true\` and \`false\` are reserved for boolean literals -- they can not
be used as identifiers.)*

**NOTE:** use of underscore (`_`) characters in names is not allowed. Hyphens
(`-`) should be used instead.

Piq *name* essentially represents the same concept as *atom* in programming
languages and correspond to either *record flags* of *variant flags (constants)*
in Piqi data model. Refer to Piqi documentation for more information.

**Examples:**

    .foo
    .bar

    .long-name

### Type name literal

Type name is a *word* starting with `':'` character. Remaining characters form a
`typename`. The most general kind of `typename` is defined as follows:

    <typename> ::= <custom-typename> | <built-in-typename>

    <built-in-typename> ::= <identifier>

    <custom-typename> ::= <module-name> '/' <identifier>

    <module-name> :: <global-module-name> | <local-module-name>

    <global-module-name> :: <domain-name> '/' <local-module-name>

    <local-module-name> ::= (<path> '/')? <piqi-file-basename>

    <domain-name> ::= lowercased Internet domain name as defined by RFC 1034

    <piqi-file-basename> ::= ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*

    <path> ::=   <path-element>
               | <path> '/' <path-element>

    <path-element ::= ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.']+

Type names correspond to some valid Piqi type. They are typically used as a part
of *Typed object* (see below) and rarely used independently.

[Piqi documentation](/doc/piqi/) provides detailed information about Piqi module
names.

**Examples:**

    % names of built-in types
    :int
    :float

    % here the module name is "piqi.org/piqtype" and the typename is "record"
    :piqi.org/piqtype/record

    % another typename from a global module
    :example.com/types/v1.0/module-foo/type-bar

    % type name of a type defined in local module "foo"
    :foo/type-bar

### Named objects

Named object is a `name` followed by some "non-name" literal.

*("Name" literal is one of the following: \`name\`, \`typename\`, \`named\` or
\`typed\`. This way "non-name" literal is any Piq literal except the above
four.)*

Named objects are used for representing both *record fields* and *variant
options*. Refer to Piqi documentation for more information.

**Examples:**

    .foo true

    .a 10

    .s "abc\n"

### Typed objects

Named object is a `typename` followed by some non-name object.

**Examples:**

    :int 10

    :float 3.14159

    :piqi.org/piqtype/record [
        .name foo
        .field [
            .name bar
            .type int
        ]
    ]

### List

**Examples:**

    []    % an empty list

    % list containing integer literals

    [ 1 2 3 4 5 ]

    % list containing lists of integers

    [ [1 2] [3 4 5] ]

    % list containing two named objects

    [ .a 0 .b 1 ]

This list represents a real (record) data structure that is taken from [person
example](/examples/#person_piq).

    [
        .name "J. Random Hacker"
        .id 0

        .email "j.r.hacker@example.com"

        .phone [ .number "(111) 123 45 67" ] % phone is "home" by default

        .phone [
            .number "(222) 123 45 67"
            .mobile
        ]

        .phone [
            .number "(333) 123 45 67"
            .work
        ]
    ]

In Piq, list is a universal representation for a container data type. Depending
on the actual Piqi data type Piq list may represent a *record* or a *list*.

Such universal container data representation has certain advantages originating
from the Piqi data model. One of the major advantages is that *record of fields*
can be also viewed as a *list of variant options*.

For example, considering the following Piqi type specification:

    .record [
        .name r
        .field [ .name a .type int ]
        .field [ .name b .type int ]
    ]

    .variant [
        .name v
        .option [ .name a .type int ]
        .option [ .name b .type int ]
    ]

    .list [ .name v-list .type v ]

the same list may be used for representing objects of both `r` and `v-list`
types:

    [ .a 0 .b 1 ]

### Record representation and parsing

As demonstrated in the previous section, Piqi record is represented as a list
that contains *named objects*, each representing a record field.

Field names, i.e. *name* parts of *named objects*, can be omitted for
convenience. However, one should keep in mind the order in which fields are
parsed, as it directly affects the way how missing field names are handled.

During record parsing, Piqi attempts to read and recognize the *required* fields
in the order they are defined in the record specification. After parsing
*required* fields, Piqi proceeds with parsing optional and repeated fields.

If Piqi can't find a required field by its name, it tries to parse the next
unrecognized record entry using the field's type specification. When parsing
succeeds, Piqi uses the result of parsing as the field's value. If parsing
fails, Piqi tries other entries until it finally succeeds to parse one of them.

Handling optional and repeated "unnamed" fields is similar to the above method,
but it introduces a fair level of uncertainty. Therefore it is recommended to
always explicitly specify names for optional and repeated fields.

### Associativity control

Parenthesis are used for overriding default associativity rules.

Since Piq is not a programming language (at least yet), their practical
application is limited to some cases when we want to define a *name literal*
which otherwise would be parsed as *named object*. (The same applies to *type
name literal* and *typed object* as well.)

For example, these expression are valid but parenthesis have no effect, since
they are applied to single objects without any ambiguous context.

    (10)

    ("foo")

    ( [1 2 3] )

    (.foo)

    (.foo bar)

These is the case, where parenthesis are necessary:

    % this is a list of two _name literals_ ".foo" and ".bar" followed by named
    % object ".fum 1"
    [ .foo .bar .fum 1 ]

    % this is a list of one _named object_ with name ".foo" and value ".bar"
    [ .foo (.bar) ]

    % this is a list of one _named object_ with name ".foo" and value
    % ".bar (.fum % 1)"
    [ .foo (.bar (.fum 1)) ]

    % this is a list of one _named object_ with name ".foo" and value ".bar (.fum)
    % followed by _integer literal_ "1".
    [ .foo (.bar (.fum)) 1 ]

### Built-in syntax abbreviation

#### Typed/Named object construction abbreviation

The named object from the previous example:

    [ .foo (.bar) ]

can also be written in a shorter form using built-in syntax abbreviation:

    [ .foo.bar ]        % NOTE: no spaces are allowed around '.'

Abbreviation can be applied multiple times:

    [ .foo (.bar (.fum 1)) ]

is equivalent to

    [ .foo.bar.fum 1 ]

This mechanism also works with *typed objects* and the mix of *typed objects*
and *named objects*. For example:

    :foo.bar 1

#### Repeated named object construction abbreviation

Suppose we need to define several *list elements* all having the same name:

    [ .foo 1 .foo 2 .foo 3 ]

Built-in macro provides a shorthand for it:

    [ (.foo 1 2 3) ]

The same idea applies to *type names* and abbreviated cons-names described in
the previous section:

    [ (:int 1 2 3) ]

    [ (:type.name a b c) ]

will be expanded to

    [ :int 1 :int 2 :int 3 ]

    [ :type.name a :type.name b :type.name c ]

Parentheses are also reserved for future control structures such as user-defined
macros and functions.

Piq file format
---------------

Piq data objects are stored in files with `.piq` extension. Each file may
contain multiple objects.

`.piq` files don't have specific headers or footers. This property gives the
ability to stream context of a `.piq` files, append `.piq` file to another one,
etc. Recognizing this property, contents of `.piq` files will be often regarded
in the current documentation as *Piq streams*.

Piq streams can contain four different kinds of entries. Their description is
given below.

-   *Typed objects.* *Typed objects* are described in above sections. They start
    with `typename` followed by an untyped data object.

-   Default type name for untyped objects.

    Piq streams allow to specify default name for those data objects that don't
    explicitly mention their type.

    `:piqtype` followed by *type name* is a special directive which sets the
    default type for subsequent untyped objects. Note that `:piqtype <smth>` is
    also a valid Piq *typed object* but here it is treated as a special case.

    `:piqtype` directives can be specified many times in a single stream. Each
    subsequent directive will override the previous one.

    Use of `:piqtype` is optional.

-   Untyped objects.

    Untyped object is a data obj that is not a typed object, that is it doesn't
    start with a typename.

    Type of untyped object is determined by the default type name specified by
    the last `:piqtype` directive or it can be provided externally (e.g. using
    `--piqtype` command-line parameter with [Piqi tools](/doc/tools/)).

-   Embedded Piqi modules.

    Embedded Piqi modules contain data type definitions. Each embedded Piqi
    module is a valid [Piqi language](/doc/piqi/) module, wrapped into
    `:piqi [ ... ]` top-level Piqi container.

    Embedded Piqi modules are treated specially. They are expected to be a valid
    Piqi specification. If they include or import other Piqi modules, they have
    to be either embedded earlier in the stream or available as external `.piqi`
    files.

    When Piq data objects that follow embedded Piqi specification are loaded,
    their type names are resolved using embedded Piqi specifications first.

Piq streams can be represented in three different formats: Piq (which is a
native format), binary and JSON. Streams can be converted from one format to
another using [Piqi tools](/doc/tools/).

Binary and JSON representation allow to conveniently and efficiently read and
write Piq data from programs. They are described [on this
page](/doc/encodings/).

**Examples:**

A simple Piq file containing three integer values:

    :piqtype int

    1 2 3

This example is equivalent to the previous one. The difference is that type for
each object is specified explicitly:

    :int 1
    :int 2
    :int 3

Piq stream can have objects of different types:

    :int 1
    :float 2.5
    :string "foo"

It is possible to have objects with the default type and with explicitly
specified type in the same stream:

    :piqtype int

    1 2 3

    :float 1.0    % "out-of-bound" object

    4 5 % some more objects with the default "int" type

    :string "bar"

Default type can be overridden by another "piqtype" directive. This feature
allows appending of one `.piq` file to another:

    % contents of file1

    :piqtype int

    1 2 3

    % contents of file2

    :piqtype float

    1.0 3.14 2.71

Note that all of the above example are based on built-in types (`int`, `float`,
`string`). Real-world `.piq` files would likely contain data objects for some
user-defined types.

There are more examples of `.piq` files on [Examples](/examples/) page.

Style guidelines
----------------

There are several code formatting rules that should be used for `.piq` and
`.piqi` files. These rules are chosen to provide better code readability and
easier code manipulation using a text editor.

For cases that are not covered in this section, refer to [Piqi pretty-printing
tool](/doc/tools/) (`piqi pp <.piq | .piqi file>` commmand).

### Indentation

Four space characters should be used for indentation.

### Parenthesis and bracket placement

If a closing bracket needs to be placed on a different line from the matching
opening bracket, it should be placed in the same column where the *named* or
*typed* or *list* object starts. The same rule applies to parentheses.

**Examples:**

    [
        ...
    ]

    .foo [
        ...
    ]

    :bar [
        ...
    ]

Notes
-----

-   There is no support for representing individual characters in Piq. And
    there's no such Piqi type as well.

-   Integer literals must fit into 64-bit unsigned range for positive values and
    for 64-bit signed range for negative values.

-   Floating point literals are internally represented in IEEE 754
    double-precision format.

-   There's no concept of *Null* in Piq/Piqi. In other languages *Null* is often
    used for representing "no value" or "undefined value". In Piq and Piqi data
    model, missing values are *missing*, meaning they are not present in
    containers and all other values are *defined*, which means that there are no
    undefined values.

-   Piq doesn't have keywords. However `true` and `false` words are reserved for
    boolean literals and can't be used in other contexts.

Known Problems
--------------

-   Piq implementation doesn't strictly follow the specification for domain
    names, path and module names -- many validity checks are missing. It means
    that Piq implementation can accept some input which is invalid according to
    this specification.

TODO list
---------

-   Line wrap for string literals.

    Currently there is no support for wrapping long string/binary literals to
    several lines.

    Once implemented it will likely look like this:

        "multi-line string \
        "another line \
        "and the last one"

-   Multi-line nested comments.

    Currently only single-line comments are supported. There are some
    implementation ideas, but more experiments and prototypes are needed before
    they become finalized.


