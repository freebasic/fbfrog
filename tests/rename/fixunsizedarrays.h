// @fbfrog -fixunsizedarrays -renametypedef A1 A2

// Internal renames due to the unsized array fixups shouldn't appear in the
// rename list.

typedef int A1;

extern int array1[];
extern char string1[];
