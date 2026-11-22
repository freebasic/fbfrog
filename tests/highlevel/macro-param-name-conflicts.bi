#pragma once

' @fbfrog -renamemacroparam Hicon Hicon_manually_renamed
' Should detect conflicts with typedefs
type HWND as long
type HICON as long
#define m0(hwnd_) f(cast(HWND, hwnd_))
' Should detect conflicts with FB keywords that are used in the macro body
#define m10(and_) (and_ and and_)
#define m11(cast_) cast(HWND, cast_)
#define m12(cptr_) cptr(long ptr, cptr_)
#define m13(integer_) cptr(integer ptr, integer_)
#define m14(_) _
#define m15(AND) (AND and AND)
#define m16(casT) cast(HWND, casT)
' Should ignore conflicts with FB keywords not used in the body
#define m20(screen) f(screen)
#define m21(val) f(val)
#define m22(len) f(len)
#define m23(string) f(string)
' Should ignore macro params conflicting with each-other,
' because fbc already detects that.
#define m30(cast, Cast) (cast(HWND, cast) + Cast)
#define m31(a, A) f(a, A)
#define m32(hwnd, Hwnd) Hwnd(cast(HWND, hwnd))
#define m33(hicon_, Hicon_manually_renamed) Hicon_manually_renamed(cast(HICON, hicon_))
