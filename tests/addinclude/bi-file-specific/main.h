// @fbfrog -emit '*/main.h' <dir>main.bi -emit '*/1.h' <dir>1.bi -emit '*/2.h' <dir>2.bi -addinclude addedmain.bi -addinclude added1.bi <dir>1.bi -addinclude added2.bi <dir>2.bi

#include "1.h"
#include "2.h"

void main(void);
