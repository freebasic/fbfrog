extern wchar_t i;
extern wchar_t *p;
extern wchar_t *p;
extern wchar_t array[10];
extern wchar_t array[10][20];
void f(wchar_t i);
void f(wchar_t *p);
void f(wchar_t array[]);
struct UDT {
	wchar_t i;
	wchar_t *p;
	wchar_t array[10];
	wchar_t array[10][20];
};
#define A1 (wchar_t)1
#define A2 (wchar_t (*)(void))1
#define A3 (void (*)(wchar_t))1

extern const wchar_t i;
extern const wchar_t *p;
extern const wchar_t *p;
extern const wchar_t array[10];
extern const wchar_t array[10][20];
void f(const wchar_t i);
void f(const wchar_t *p);
void f(const wchar_t array[]);
struct UDT {
	const wchar_t i;
	const wchar_t *p;
	const wchar_t array[10];
	const wchar_t array[10][20];
};
#define A4 (const wchar_t)1
#define A5 (const wchar_t (*)(void))1
#define A6 (void (*)(const wchar_t))1
