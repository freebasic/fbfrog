#pragma once

'' The following symbols have been renamed:
''     #define I3_ alias "I3"
''     #define I4_ alias "I4"
''     constant I5_ alias "I5"
''     constant I6_ alias "I6"
''     #define I7_ alias "I7"
''     #define I8_ alias "I8"

extern "C"

#define I1() 1

declare sub fi1(byval i1_ as long)
declare sub fi2(byval i2 as long)

#define I2() 1
#define I3_() 1

declare sub i3()
declare sub i4()

#define I4_() 1

enum
	I5_
end enum

declare sub i5()
declare sub i6()

enum
	I6_
end enum

#define I7_ 1

declare sub i7()
declare sub i8()

#define I8_ 1

end extern
