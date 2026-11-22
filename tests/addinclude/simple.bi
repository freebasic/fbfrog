#pragma once

#include once "foo.bi"

extern "C"

' @fbfrog -addinclude foo.bi
declare sub simple()

end extern
