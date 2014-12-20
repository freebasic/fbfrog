// @fbfrog -incdir <dir> -include directinclude-1.h -include directinclude-2.h -filterout '*'
// Direct #includes of files with #include guards should be preserved when filtering out
#include "directinclude-1.h"
#include "directinclude-2.h"
