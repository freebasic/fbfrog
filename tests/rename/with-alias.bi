#pragma once

'' The following symbols have been renamed:
''     variable a => b

extern "C"

' @fbfrog -rename a b
' fbfrog should be able to rename symbols even if they already have an alias,
' and the renamelist should mention the original C identifier, not the alias...
extern b alias "c" as long

end extern
