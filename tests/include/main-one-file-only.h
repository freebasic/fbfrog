// @fbfrog -emit '*/2.h' <dir>main-one-file-only.bi

static int a;
#include "1.h"
#include "2.h"
#include "3.h"
static int b;
