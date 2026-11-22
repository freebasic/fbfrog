#pragma once

' @fbfrog -undefbeforedecl A -undefbeforedecl B
#undef A
const A = 1
#undef B
