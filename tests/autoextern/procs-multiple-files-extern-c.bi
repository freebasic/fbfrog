#pragma once

extern "C"

'' The following symbols have been renamed:
''     procedure f1_ alias "f1"()
''     procedure f2_ alias "f2"()
''     procedure f3_ alias "f3"()
''     procedure f4_ alias "f4"()
''     procedure f5_ alias "f5"()
''     procedure f6_ alias "f6"()
''     procedure f7_ alias "f7"()
''     procedure f8_ alias "f8"()
''     procedure f9_ alias "f9"()

declare sub f1()
declare sub f2()
declare sub f3()
declare sub f4()
declare sub f5 stdcall()
declare sub f6()
declare sub f7()
declare sub f8()
declare sub f9()
declare sub f1_ alias "f1" stdcall()
declare sub f2_ alias "f2" stdcall()
declare sub f3_ alias "f3" stdcall()
declare sub f4_ alias "f4" stdcall()
declare sub f5_ alias "f5"()
declare sub f6_ alias "f6" stdcall()
declare sub f7_ alias "f7" stdcall()
declare sub f8_ alias "f8" stdcall()
declare sub f9_ alias "f9" stdcall()

end extern
