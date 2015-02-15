// @fbfrog -emit '*/1.h' <dir>main-nested-only-1.bi -emit '*/2.h' <dir>main-nested-only-2.bi

static int a;
#include "include1.h"
#include "include2.h"
static int b;
