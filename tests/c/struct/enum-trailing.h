// Trailing comments on the final (comma-less) enumerator
enum E1 {
	A1,
	B1 = 2 // trailing on last
};

typedef enum {
	C1,
	D1 = 4 /* trailing block comment on last */
} E2;

enum E3 { E3A, E3B = 7, };