#pragma once

extern "C"

declare sub f()
declare function f(byval as long, byval as long) as long
declare sub bar()
declare sub baz()
declare sub bar()
declare sub baz()
declare sub bar()
declare sub baz()
declare sub f()
declare sub f()
'' TODO: #define m1(...) void f(__VA_ARGS__)
'' TODO: #define m2(args...) void f(args)
declare sub f()
declare sub f()
declare sub f(byval as long)
declare sub f(byval as long)
declare sub f(byval as long, byval as long)
declare sub f(byval as long, byval as long)
declare sub f(byval as single, byval as double, byval as long, byval as long)
declare sub f(byval as single, byval as double, byval as long, byval as long)
#undef m1
#undef m2
'' TODO: #define m1(...) void f(void (*p) __VA_ARGS__)
'' TODO: #define m2(args...) void f(void (*p) args)
declare sub f(byval p as sub())
declare sub f(byval p as sub())
declare sub f(byval p as sub(byval as long))
declare sub f(byval p as sub(byval as long))
declare sub f(byval p as sub(byval as long, byval as long))
declare sub f(byval p as sub(byval as long, byval as long))
#undef m1
#undef m2
'' TODO: #define m1(a, ...) void a(__VA_ARGS__)
'' TODO: #define m2(a, args...) void a(args)
declare sub f()
declare sub f()
declare sub f(byval as long)
declare sub f(byval as long)
#undef m1
#undef m2
'' TODO: #define m1(a, b, ...) a b(__VA_ARGS__)
'' TODO: #define m2(a, b, args...) a b(args)
declare sub f()
declare sub f()
declare sub f(byval as long)
declare sub f(byval as long)
#undef m1
#undef m2
'' TODO: #define m1(a, ...) void a(__VA_ARGS__)
declare sub x(byval as long)
declare sub x()
declare sub x(byval as long, byval as long)
declare sub x(byval as long)
declare sub x()
#undef m1

end extern
