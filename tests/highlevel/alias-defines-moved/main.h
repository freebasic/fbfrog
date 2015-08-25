// @fbfrog -emit '*/main.h' <dir>main.bi -emit '*/other1.h' <dir>other1.bi -emit '*/other2.h' <dir>other2.bi

#include "other1.h"

#define A1 123

#include "other2.h"
