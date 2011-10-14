// type T : ... : end type
// type as T A
// (Both ids might be needed)
typedef struct T {
	int a;
} A;

// Anonymous struct typedef
// type TT : ... : end type
typedef struct {
	int a;
} TT;

// Anonymous struct typedef triggering the fake id insertion
typedef struct {
	int a;
} A, *PA;

// type T : ... : end type
// type as T A, B : type as T ptr C : type as function() as T D
typedef struct T {
	int a;
} A, B, *C, (*D)();

// type T : ... : end type
// (also, any places using <struct T> will become just <T>, so they work ok)
struct T {
	int a;
};

typedef struct { int a; } T;

struct T { int a; };
