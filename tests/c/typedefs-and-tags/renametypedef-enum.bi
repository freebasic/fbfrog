#pragma once

'' The following symbols have been renamed:
''     typedef A => myint

' @fbfrog -renametypedef A myint

enum A
	FOO
end enum

type myint as long
dim shared x1 as A
dim shared x2 as myint
