// should become
//    typedef struct a ## b;
// not
//    typedef struct ab;
#define EXPANDME1(param) param
typedef struct EXPANDME1(a ## b);
