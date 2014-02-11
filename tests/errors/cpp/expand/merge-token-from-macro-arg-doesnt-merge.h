// should become
//    typedef struct a ## b;
// not
//    typedef struct ab;
#define EXPANDME1(param) typedef struct param;
EXPANDME1(a ## b)
