## Command Line Options

#### Global options

* `@<file> | *.fbfrog`: Read more command line arguments from a file
* `-o <path/file>`: Set output .bi file name, or just the output directory
* `-v`: Show verbose/debugging info. Pass `-v -v` for more debug info.
* `-target nodos|noarm|<os>|<arch>|<os>-<arch>`: Specify OS/arch to translate for, instead of all.

#### CPP (options are API-specific)

* `-define <id> [<body>]`: Add pre-#define
* `-include <file>`: Add pre-#include
* `-incdir <path>`: Add search directory for .h #includes

#### Binding generation (options are API-specific)

* `-windowsms`: Use Extern "Windows-MS" instead of Extern "Windows"
* `-clong32`: Translate C long as 32bit LONG, instead of CLONG
* `-fixunsizedarrays`: Wrap `[]` arrays with a #define
* `-nofunctionbodies`: Don't preserve function bodies
* `-dropmacrobodyscopes`: Drop scope blocks with only 1 statement in macro bodies
* `-replacements <file>`: Load patterns for search/replace
* Options for renaming symbols (`-rename* <oldid> <newid>`):
    * `-renametypedef`, `-renametag` (struct/union/enum)
    * `-renameproc` (procedures)
    * `-renamedefine`, `-renamemacroparam`
    * `-rename` (any matching symbol)
* `-removeEmptyReservedDefines`: Remove empty (and parameter-less) #defines with `__*` or `_U*` names
* `-rename_ <id>`: Rename symbol by appending an _ underscore
* Options for removing declarations (`-remove* <id>`, where `<id>` is an id or a pattern):
    * `-removedefine`, `-removeproc`, `-removevar`, `-remove1st`, `-remove2nd`
    * `-remove` (any matching symbol)
* `-dropprocbody <id>`: Don't preserve a certain procedure's body
* `-typedefhint <id>`: Mark symbol as typedef, to help parsing of type casts
* `-addforwarddecl <id>`: Force a forward declaration to be added for the given type
* `-undefbeforedecl <id>`: Insert an #undef above a declaration
* `-ifndefdecl <id>`: Wrap declaration of symbol named <id> in an #ifndef block
* `-convbodytokens <id>`: Translate a #define's body only by converting the tokens, no parsing
* `-forcefunction2macro <id>`: Force an inline function to be converted to a macro, even if parameters are used multiple times
* `-expandindefine <id>`: Expand macro in #define body
* `-noexpand <id>`: Disable expansion of certain #define
* `-expand <id>`: Expand and remove matching typedefs
* `-nostring <decl-pattern>`: Prevent a symbol from being turned into a zstring
* `-string <decl-pattern>`: Force a `[U]Byte [Ptr]` symbol to be turned into a `ZString [Ptr]`
* `-removeinclude <filename>`: Remove matching #include directives
* `-setarraysize <id> <size>`: Set size of an `[]` array
* `-moveabove <id> <ref>`: Move declaration of `<id>` above declaration of `<ref>`
