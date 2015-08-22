// Macro expansion should not be done inside the <...> part of an #include
#define foo 1
#include <foo/1.h>  // foo/1.h

// unless it's a computed #include, then all tokens are expanded, even inside
// the <...>
#define filename <foo/2.h>
#include filename  // 1/2.h
