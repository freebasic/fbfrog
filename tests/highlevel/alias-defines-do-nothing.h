// Sometimes we don't know which declaration an alias define refers to.
// In such cases it must not be turned into a declaration.

struct A { int i; };
void A(void);
#define A_ A

struct B { int i; };
#define B_ B
void B(void);

#define C_ C
struct C { int i; };
void C(void);

static int SV1;
#define SV2 SV1
