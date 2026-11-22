#pragma once

'' The following symbols have been renamed:
''     typedef MY_INTEGER => MY_INTEGER_

extern "C"

' @fbfrog -renamedefine MY_INTEGER MY_INTEGER_
' If adding a forward declaration for a typedef that was renamed, the forward
' declaration should appear in the rename list, instead of the typedef.
type MY_INTEGER_ as MY_INTEGER__
declare sub f(byval p as MY_INTEGER_ ptr)
type MY_INTEGER__ as longint
type my_Integer as longint

end extern
