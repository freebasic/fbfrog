#pragma once

#include once "bispecific-1.bi"

extern "C"

' @fbfrog -emit '*/bispecific.h' <dir>bispecific.bi -emit '*/bispecific-1.h' <dir>bispecific-1.bi -title "bispecific 1.0" <dir>example2.h <dir>example3.h <dir>bispecific-1.bi
declare sub bispecific()

end extern
