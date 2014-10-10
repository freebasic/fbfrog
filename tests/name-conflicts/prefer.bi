#pragma once

'' The following symbols have been renamed:
''     #define I3 => I3_
''     #define I4 => I4_
''     constant I5 => I5_
''     constant I6 => I6_
''     #define I7 => I7_
''     #define I8 => I8_

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
