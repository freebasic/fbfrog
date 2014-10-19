// @fbfrog -filterout '*'
// forwarddecl.h contains a declaration using an undeclared tag id. We have to
// add a forward declaration for it, but not here, because here it's being
// filtered out.
#include "forwarddecl.h"
