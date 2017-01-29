## Extra features

fbfrog has various options for manual improvements, for example for...
* Renaming symbols, which is useful to resolve name conflicts.
  This is a common problem due to FB's case-insensitivity and different namespacing (e.g. #defines collide with functions).
  When using the renaming options, auto-generated list of renamed symbols are added to the top of affected header files.
* Translating #define bodies as token sequences, instead of trying to parse as expression (which is useful in specific cases).
* Removing declarations by name or type.
* Expansion of any typedef by name
* Specifying the size of unsized arrays
* Specifying hints about which identifiers are typedefs, which helps the C parser's
  type cast expression parsing when not all typedefs were declared yet. Sometimes C headers
  use typedefs from other headers, so fbfrog may not get to see the typedef declarations.
  Or a typedef may be used in a #define body before being declared.
* etc., run `./fbfrog` to see list of options

In general, this covers things that can't be decided automatically, but require human intervention.
