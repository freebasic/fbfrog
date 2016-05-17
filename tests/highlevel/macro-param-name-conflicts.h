// Should warn about conflitcs with typedefs
typedef int HWND;
#define m1(hwnd) f((HWND)hwnd)

// Should warn about conflicts with FB keywords that are used in the macro body
#define m2(and) (and & and)
#define m3(cast) ((HWND)cast)
#define m4(cptr) ((int*)cptr)
#define m5(long) ((int*)long)
#define m6(integer) ((ssize_t*)integer)
#define m7(_) _

// Should not warn about conflicts with other, unused FB keywords
#define m8(screen) f(screen)
#define m9(val) f(val)
#define m10(len) f(len)
#define m11(string) f(string)
