// Other macro called in arg, arg sometimes used with # and ##
#define EXPANDME1(a) \
	void f1(a p##a[32] = #a); \
	void f2(a a##p[32] = #a); \
	void f3(a p##a##x[32] = #a);
#define EXPANDME2 char
// void f1(char pEXPANDME2[32] = "EXPANDME2");
// void f2(char EXPANDME2p[32] = "EXPANDME2");
// void f3(char pEXPANDME2x[32] = "EXPANDME2");
EXPANDME1(EXPANDME2)
