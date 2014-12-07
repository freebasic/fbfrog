void A(int i);

#define A(i) A(i+1)

static int i = A(0);
