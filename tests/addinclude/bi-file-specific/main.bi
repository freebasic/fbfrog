#pragma once

#include once "1.bi"
#include once "2.bi"
#include once "addedmain.bi"

extern "C"

' @fbfrog -emit '*/main.h' <dir>main.bi -emit '*/1.h' <dir>1.bi -emit '*/2.h' <dir>2.bi -addinclude addedmain.bi -addinclude added1.bi <dir>1.bi -addinclude added2.bi <dir>2.bi
declare sub main()

end extern
