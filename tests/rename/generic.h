// @fbfrog -rename E E_ -rename A A_ -rename B B_ -rename f f_ -rename p p_ -rename i i_ -rename T T_

enum E {
	A = 0
};

typedef enum E T;

#define B A

T f(T p = B);

extern T i;
