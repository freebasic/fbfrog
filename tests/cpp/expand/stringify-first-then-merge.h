// @fbfrog -removedefine m

#define m(x) L ## #x

void f1(wchar_t *p = m(a));
void f2(wchar_t *p = L"a");
