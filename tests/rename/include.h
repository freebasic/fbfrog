// @fbfrog -rename foo.h bar.h

#ifdef _WIN32
	#include "foo.h"
#else
	#include "bar.h"
#endif
