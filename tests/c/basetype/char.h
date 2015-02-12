extern char i;
extern char *p;
extern char *p;
extern char array[10];
extern char array[10][20];
void f(char i);
void f(char *p);
void f(char array[]);
struct UDT {
	char i;
	char *p;
	char array[10];
	char array[10][20];
};
#define A1 (char)1
#define A2 (char (*)(void))1
#define A3 (void (*)(char))1

extern const char i;
extern const char *p;
extern const char *p;
extern const char array[10];
extern const char array[10][20];
void f(const char i);
void f(const char *p);
void f(const char array[]);
struct UDT {
	const char i;
	const char *p;
	const char array[10];
	const char array[10][20];
};
#define A4 (const char)1
#define A5 (const char (*)(void))1
#define A6 (void (*)(const char))1

extern signed char i;
extern signed char *p;
extern signed char array[10];
extern signed char array[10][20];
void f(signed char i);
void f(signed char *p);
void f(signed char array[]);
struct UDT {
	signed char i;
	signed char *p;
	signed char array[10];
	signed char array[10][20];
};
#define A7 (signed char)1
#define A8 (signed char (*)(void))1
#define A9 (void (*)(signed char))1

extern const signed char i;
extern const signed char *p;
extern const signed char array[10];
extern const signed char array[10][20];
void f(const signed char i);
void f(const signed char *p);
void f(const signed char array[]);
struct UDT {
	const signed char i;
	const signed char *p;
	const signed char array[10];
	const signed char array[10][20];
};
#define A10 (const signed char)1
#define A11 (const signed char (*)(void))1
#define A12 (void (*)(const signed char))1

extern unsigned char i;
extern unsigned char *p;
extern unsigned char array[10];
extern unsigned char array[10][20];
void f(unsigned char i);
void f(unsigned char *p);
void f(unsigned char array[]);
struct UDT {
	unsigned char i;
	unsigned char *p;
	unsigned char array[10];
	unsigned char array[10][20];
};
#define A13 (unsigned char)1
#define A14 (unsigned char (*)(void))1
#define A15 (void (*)(unsigned char))1

extern const unsigned char i;
extern const unsigned char *p;
extern const unsigned char array[10];
extern const unsigned char array[10][20];
void f(const unsigned char i);
void f(const unsigned char *p);
void f(const unsigned char array[]);
struct UDT {
	const unsigned char i;
	const unsigned char *p;
	const unsigned char array[10];
	const unsigned char array[10][20];
};
#define A16 (const unsigned char)1
#define A17 (const unsigned char (*)(void))1
#define A18 (void (*)(const unsigned char))1
