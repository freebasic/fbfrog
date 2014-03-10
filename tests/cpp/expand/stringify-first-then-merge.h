#define EXPANDME1(x) L ## #x

void f1(wchar_t *p = EXPANDME1(a));
void f2(wchar_t *p = L"a");
