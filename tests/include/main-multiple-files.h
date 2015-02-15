// @fbfrog -emit '*/1.h' <dir>main-multiple-files-1.bi -emit '*/2.h' <dir>main-multiple-files-2.bi -emit '*/3.h' <dir>main-multiple-files-3.bi -emit '*/include1.h' <dir>main-multiple-files-include1.bi -emit '*/includenotfound.h' <dir>main-multiple-files-includenotfound.bi

static int a;
#include "1.h"
#include "include2.h"
#include "3.h"
#include "include1.h"
#include "includenotfound.h"
static int b;
