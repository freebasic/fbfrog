#pragma once

'' The following symbols have been renamed:
''     procedure f1 => f1_
''     procedure f2 => f2_
''     procedure f3 => f3_
''     procedure f4 => f4_
''     procedure f5 => f5_
''     procedure f6 => f6_
''     procedure f7 => f7_
''     procedure f8 => f8_
''     procedure f9 => f9_

extern "C"

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
