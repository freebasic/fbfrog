#define merge(a,b) a##b
#define stringify(x) #x

// 08 is not a valid number literal, but it's a valid preprocessing token
static int x1 = merge(1, 08);

static char x2[32] = stringify(111aaazzz);
static char x3[32] = stringify(1e+1e+2);
