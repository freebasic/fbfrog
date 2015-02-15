// @fbfrog -dontemit '*/2.h' -emit '*' <dir>main-dontemit.bi

static int a;
#include "1.h"
#include "2.h"
#include "3.h"
static int b;
