#pragma once

type HWND as long
type HICON as long
#define m0(hwnd) f(clng(hwnd))
#define m10(and_) (and_ and and_)
#define m11(cast) clng(cast)
#define m12(cptr_) cptr(long ptr, cptr_)
#define m13(integer_) cptr(integer ptr, integer_)
#define m14(_) _
#define m15(AND) (AND and AND)
#define m16(casT) clng(casT)
#define m20(screen) f(screen)
#define m21(val) f(val)
#define m22(len) f(len)
#define m23(string) f(string)
#define m30(cast, Cast) (clng(cast) + Cast)
#define m31(a, A) f(a, A)
#define m32(hwnd, Hwnd) Hwnd(clng(hwnd))
#define m33(hicon, Hicon_manually_renamed) Hicon_manually_renamed(clng(hicon))
