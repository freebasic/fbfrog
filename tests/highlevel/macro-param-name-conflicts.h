// @fbfrog -renamemacroparam Hicon Hicon_manually_renamed

// Should detect conflicts with typedefs
typedef int HWND;
typedef int HICON;
#define m0(hwnd) f((HWND)hwnd)

// Should detect conflicts with FB keywords that are used in the macro body
#define m10(and) (and & and)
#define m11(cast) ((HWND)cast)
#define m12(cptr) ((int*)cptr)
#define m13(integer) ((ssize_t*)integer)
#define m14(_) _
#define m15(AND) (AND & AND)
#define m16(casT) ((HWND)casT)

// Should ignore conflicts with FB keywords not used in the body
#define m20(screen) f(screen)
#define m21(val) f(val)
#define m22(len) f(len)
#define m23(string) f(string)

// Should ignore macro params conflicting with each-other,
// because fbc already detects that.
#define m30(cast, Cast) ((HWND)cast + Cast)
#define m31(a, A) f(a, A)
#define m32(hwnd, Hwnd) Hwnd((HWND)hwnd)
#define m33(hicon, Hicon) Hicon((HICON)hicon)
