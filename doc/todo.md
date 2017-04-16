## Old to-do list

* -1to1 option which automatically adds -emit options for each input .h such
  that each .h is emitted into its own .bi, in the directory given with -o. Strip only the common prefix, preserve remaining directory structure (if any).
* Define2Decl shouldn't move all alias defines - it's typically unnecessary for procs/vars/typedefs at least.
* Define2Decl shouldn't count #undefs as declarations (preventing affected symbols from being handled by the pass)
* Define2Decl should count multiple, equal declarations as one declaration
* Only add things to renamelist if they have a RENAMED flag (not everything with an alias was renamed)
* Add -printcconstruct <pattern> option for dumping C constructs as seen by fbfrog
  to make writing replacements easier. (TODOs aren't enough, because sometimes we
  want to do a replacement even though it's not a TODO)
* don't build VERAND conditions at frogEvaluateScript() time, but rather do it
  later when generating the #if conditions. -declareversions/-declarebool should store version number/flags in ApiInfo,
  and frogEvaluateScript() should build ApiInfo objects directly, then copy them for recursive invocations, no more separate loadOptions().
* LCS algorithm is main performance bottle-neck (especially for Windows API binding), can it be optimized?
* Turn more inline functions into macros: also void functions whose body can
  just be used as macro'd scope block, and doesn't contain any RETURNs.
* It would be nice if fbfrog could preserve comments for documentation purposes.
* Flatten AST data structure such that statements nested inside struct/proc bodies
  can be merged separately from the compound block (i.e. use TYPEBEGIN/TYPEEND/PROCBEGIN/PROCEND nodes).
  Interesting for merging the fields if UDT's FIELD=N value changes between targets.
* Don't expand macro constants outside CPP expressions, to keep them as array size etc.
* Solve out tag ids if there is an alias typedef, unless the tag id is used elsewhere
* Add pattern-based renames, e.g. `-renamedefine '%' 'FOO_%'`,
  or at least `--rename-define-add-prefix '*' FOO_` (add prefix FOO_ to matching defines).
* Auto-convert C's [] array indexing into FB's (): track which vars/fields are
  arrays (or pointers) and then compare indexing BOPs against that.
* Add support for `#pragma pack` with named stack entries (`#pragma {pack|pop}(push, <identifier> [, N])`)
  Popping by name means popping everything until that node is popped. If not found, nothing is popped.
  (MinGW-w64 CRT headers use this)
* Continue support for parsing function bodies: `++` and `--` operators, for loops, continue/break, goto/labels/switch/case.
* Reference typedefs - must be expanded like array typedefs
* Add more C++ support
* ...
