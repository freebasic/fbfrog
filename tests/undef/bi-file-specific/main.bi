#pragma once

#undef main

#include once "1.bi"
#include once "2.bi"

extern "C"

' @fbfrog -emit '*/main.h' <dir>main.bi -emit '*/1.h' <dir>1.bi -emit '*/2.h' <dir>2.bi -undef main -undef 1 <dir>1.bi -undef 2 <dir>2.bi
declare sub main()

end extern
