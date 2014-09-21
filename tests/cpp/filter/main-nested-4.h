// @fbfrog -filterout '*include1.h' -filterout '*include2.h'

static int a;
#include "include1.h"
#include "include2.h"
static int b;
