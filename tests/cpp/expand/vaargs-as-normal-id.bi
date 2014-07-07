#pragma once

extern "C"

'' The following symbols have been renamed:
''     procedure __VA_ARGS___ alias "__VA_ARGS__"()

#define m1 __VA_ARGS___
#define m2(x) x##__VA_ARGS___

declare sub __VA_ARGS__()
declare sub __VA_ARGS___ alias "__VA_ARGS__"()
declare sub aaa__VA_ARGS__()

end extern
