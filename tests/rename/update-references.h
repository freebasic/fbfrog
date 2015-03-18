// @fbfrog -renametag A1 A2 -renametypedef B1 B2 -renamedefine C1 C2

struct A1 {
	int i;
};

typedef int B1;

#define C1 123

// When the above symbols are renamed, these references to them should be updated too
#define Ref1 struct A1
#define Ref2 B1
#define Ref3 C1
extern struct A1 Ref4;
extern B1 Ref5;
