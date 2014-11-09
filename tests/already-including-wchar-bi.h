// If a header already #includes crt/wchar.bi, then fbfrog shouldn't add a
// duplicate #include because of declarations using wchar_t.

#include <wchar.h>

extern wchar_t x;
