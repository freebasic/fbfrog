// @fbfrog -filterout 'tests?filter?include1.h'

static int a;
#include "includeinclude1.h"
#include "includenotfound.h"
static int b;
