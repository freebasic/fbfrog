extern "C"

const A1 = 1
const A2 = 7

#define B1(x) x
#define B2 x
#define B3
#define C1 x

declare sub cool2(byval as long)

#define C2 cool2(1)

end extern
