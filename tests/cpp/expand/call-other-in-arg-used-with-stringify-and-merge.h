// @fbfrog -removedefine m1 -removedefine m2

// Other macro called in arg, arg sometimes used with # and ##
#define m1(a) \
	void f1(a p##a[32] = #a); \
	void f2(a a##p[32] = #a); \
	void f3(a p##a##x[32] = #a);
#define m2 char
m1(m2)
void f1(char pm2[32] = "m2");
void f2(char m2p[32] = "m2");
void f3(char pm2x[32] = "m2");
