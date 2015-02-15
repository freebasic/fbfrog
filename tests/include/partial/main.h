// @fbfrog -emit '*/main.h' <dir>main.bi

/* It doesn't make sense to filter out #includes which are needed to turn
   partial constructs into complete ones. The only thing that can realistically
   be done with such #include statements is to expand them in-place.

   Such include files (those that contain some part of a construct, perhaps just
   an arbitrary token sequence, but not a complete construct) can't even be
   translated. That would probably not even be useful.

   But currently fbfrog does not complain about it, and thus, it at least should
   produce working code if it happens. */

struct UDT {
	#include "field.h"
};

void f(
	#include "param.h"
);
