// @fbfrog -renametag A1 B1 -renametag A2 B2 -renametag A3 B3 -renametag A4 B4

struct a { int f; };
struct A { int f; };

struct a1 { int f; };
struct A1 { int f; };

struct a2 { int f; };
struct A2 { int f; };

struct UDT1 {
	struct A3 { int f; };
};

union UDT2 {
	struct A4 { int f; };
};
