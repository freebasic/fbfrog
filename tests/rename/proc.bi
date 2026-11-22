#pragma once

'' The following symbols have been renamed:
''     procedure A1 => A2

extern "C"

' @fbfrog -renameproc A1 A2
declare sub A2 alias "A1"()
' procptr subtypes shouldn't cause issues
extern p as sub()

end extern
