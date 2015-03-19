// @fbfrog -emit '*/main.h' <dir>main.bi -emit '*/1.h' <dir>1.bi -emit '*/2.h' <dir>2.bi -undef main -undef 1 <dir>1.bi -undef 2 <dir>2.bi

#include "1.h"
#include "2.h"

void main(void);
