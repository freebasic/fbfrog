// @fbfrog -emit '*/removeinclude.h' <dir>removeinclude.bi -removeinclude 2.h -removeinclude does-not-exist.h

#include "1.h"
#include "2.h"
#include "does-not-exist.h"
