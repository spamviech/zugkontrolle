# kommandozeilen_argumente

__Anmerkung__: Dies ist die englische ReadMe, für die deutsche Version siehe
[LIESMICH.md](https://github.com/spamviech/kommandozeilen_argumente/blob/main/LIESMICH.md).

TODO english version

Parser for command line arguments with optional automatic help generation.

Arguments are created with the provided associated functions,
the result type is specified with a type variable.
Arguments can be combined to more complex structures using more than one argument
with the `combine!` macro, or one of the dedicated `combineN` functions.

Arguments are identified by their long names and potentially their short names.
Long names are usually given after two minus characters `--long`.
Short names are usually given after one minus character `-short`.
Short names are expected to consist of only one
[Grapheme](https://docs.rs/unicode-segmentation/1.8.0/unicode_segmentation/trait.UnicodeSegmentation.html#tymethod.graphemes).

All Strings can be adjusted, e.g. description of an argument in the help message.
Specialized functions for a german and english version are available if it is relevant.
Additionally, german an english synonyms are available.
Avoiding repetition of strings can be achieved using the `Language` type.

An Argument can have a default value, which is used if none of its names are used.
Without a default value the argument must be used, resulting in a parse error otherwise.

## Flags

Arguments without values are called flags, they can be active or inactive.
In most cases they represent `bool` arguments, but other types are also supported.

A flag with long name `flag`, long_prefix `--`, short name `f`, short_prefix `-`,
invert_prefix `no` and invert_prefix `-` can be activated with `--flag` or `-f`
and deactivated with `--no-flag`.

If several flags with short names `f`, `g` and `h` exist they can be activated together with `-fgh`.

### Early Exit

A special kind of flag argument causing an early exit.
They can't be deactivated and cause an early exit with an associated message.
Typical use cases are displaying of the current version or the help text.

## Values

Arguments can specify values.
When specified with a long name, the value can be given after a space or the `=` character.
Using a short name it is possible to specify it directly after the name.
The value is parsed from a `OsString`.

It is possible to show all allowed values in the generated help text.

A value argument with long name `value`, long_prefix `--`, short name `v`, short_prefix `-`
and value_infix `=` for a number type is parsed with result `3` for each of the following inputs:

- `--value 3`
- `--value=3`
- `-v 3`
- `-v=3`
- `-v3`

## Feature "derive"

Accepted command line arguments can be produced automatically using the `derive` feature.
It allows deriving an implementation of the `Parse` trait for a `struct` with named fields.

The long name of the argument is the field name,
the description in the help text is the docstring of the field.

The argument is parsed according to the `ParseArgument` trait.
Instances exist for `bool`, `String`, number types (`i8`, `u8`, `i16`, `u16`, ..., `f32`, `f64`),
`Option<T>` and instances of the `EnumArgument` trait.
`bool` fields produce flag arguments which are off by default.
Every other (provided) type produces a value argument; `Option<T>` has default value `None`,
all other types produce required arguments.
It is possible to derive an implementation of the `EnumArgument` trait for `enum` types holding no data.
Types used as a `ParseArgument` must be an instance of `Display`.

The default behaviour can be changed using `#[kommandozeilen_argumente(<Optionen>)]` attributes.

The following options are supported at the `struct` declaration.

- `sprache: <sprache>` | `language: <language>`:
  Default value for some strings, default: `english`.
  Builtin languages for `deutsch`, `englisch` and `english`.
- `version`: create a `--version`, `-v` flag.
- `hilfe` | `help`: create a help text flag.
- `hilfe(<opts>)`, `help(<opts>)`, `version(<opts>)`:
  Similar to the variant without ops, but short name is off by default. Possible Opts:
  - `lang_präfix: <präfix>` | `long_prefix: <prefix>`: Prefix before long name.
  - `lang: <name>`, `long [<namen>]`: Overwrite long name.
  - `kurz_präfix: <präfix>` | `short_prefix: <prefix>`: Prefix before short name.
  - `kurz`: Set short name as first Grapheme of the first long name.
  - `kurz: <name>`, `kurz: [<namen>]`: Overwrite short name.
  - `sprache: <sprache>` | `language: <language>`: Language of the help text and default names.
  - `beschreibung: <programm-beschreibung>` | `description: <program_description>`:
    Only supported for `hilfe(<opts>)` and `help(<opts>)`.
    Set the program description shown in the help text.
- `case: sensitive`, `case: insensitive`:
  All Strings (Names, prefix, infix) are parse case-sensitive or -insensitive,
  default: `case: sensitive`.
- `case(<opts>)`:
  Specific control about case-sensitivity. All opts have the form `<name>: <value>`,
  `<value>` must be one of `sensitive` and `insensitive`. Allowed Names:
  - `lang_präfix` | `long_prefix`
  - `lang` | `long`
  - `kurz_präfix` | `short_prefix`
  - `kurz` | `short`
  - `invertiere_präfix` | `invert_prefix`
  - `invertiere_infix` | `invert_infix`
  - `wert_infix` | `value_infix`
- `lang_präfix: <präfix>` | `long_prefix: <prefix>`:
  Overwrite default value for prefix before long names, default: `--`.
- `kurz_präfix: <präfix>` | `short_prefix: <prefix>`:
  Overwrite default value for prefix before short names, default: `-`.
- `invertiere_präfix: <string>` | `invert_prefix: <string>`:
  Overwrite default value for prefix to invert a flag, default: `kein` or `no`.
- `invertiere_infix: <string>` | `invert_infix: <string>`:
  Overwrite default value for infix after prefix to invert a flag, default `-`.
- `wert_infix: <string>` | `value_infix: <string>`:
  Overwrite default value for infix to give a value in the same argument, default `=`.
- `meta_var: <string>` | `meta_var: <string>`:
  Overwrite default value for the meta variable shown in the help text, default: `WERT` or `VALUE`.

Field support the following options:

- `glätten`/`flatten`: Use the `Parse` trait (include the configured arguments).
- `FromStr`: Use the `FromStr` trait (`Display` instance required for both value and error type).
- `benötigt`/`required`: Don't use the configured default value.
- `lang_präfix: <präfix>` | `long_prefix: <prefix>`: Prefix before long name.
- `lang: <name>` | `long: <name>`: Overwrite long name.
- `lang: [<namen>]` | `long: [<names>]`: Set multiple long names (comma separated list).
- `kurz_präfix: <präfix>` | `short_prefix: <prefix>`: Prefix before short name.
- `kurz`/`short`: Set short name as first Grapheme of the first long name.
- `kurz: <wert>"`/`short: <value>"`: Overwrite the short name.
- `kurz: [<namen>]` | `short: [<names>]`: Set multiple short names (comma separated list).
- `standard: <wert>` | `default: <value>`: Overwrite default value.
- `invertiere_präfix: <string>` | `invert_prefix: <string>`: Overwrite prefix to invert a flag.
- `invertiere_infix: <string>` | `invert_infix: <string>`:
  Overwrite infix after prefix to invert a flag.
- `wert_infix: <string>` | `value_infix: <string>`:
  Overwrite infix to give the value in the same argument.
- `meta_var: <string>`: Overwrite meta variable used in the help text.

## Example

A simple example for a `struct` with 3 flags and 2 value, created using the
[function API](https://github.com/spamviech/kommandozeilen_argumente/blob/main/examples/function.rs)
and the
[derive API](https://github.com/spamviech/kommandozeilen_argumente/blob/main/examples/derive_en.rs)
are available in the
[GitHub repository](https://github.com/spamviech/kommandozeilen_argumente/).
Both cases produce the following help text:

```cmd
kommandozeilen_argumente 0.2.0
program description.

derive_en.exe [OPTIONS]

OPTIONS:
  --[no]-flag                         A flag with default settings. [Default: false]
  --[no]-(other|names)  | -u          A flag with alternative names. [Default: false]
  --[kein]-required     | -r          A flag without default value, with alternative prefix to invert the flag.
  --value(=| )VALUE                   A String value.
  --enumeration(=| )VAR | -e[=| ]VAR  An Enumeration-value with default value and alternative meta variable. [Possible values: One, Two, Three | Default: Two]
  --version             | -v          Show the current version.
  --help                | -h          Show this text.
```

## Missing (planned) Features

- subcommands
- position-based arguments
- Different default value for name without value and name doesn't appear
- argument-groups (only one of these N flags can be active)
