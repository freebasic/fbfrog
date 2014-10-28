// @fbfrog -v
// should become
//    typedef struct a ## b;
// not
//    typedef struct ab;
#define m(param) param
typedef struct m(a ## b);
