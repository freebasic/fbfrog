#define m1 x1
#define m2 x2
#define m3(x) m1##x

// m2 shouldn't be expanded in the arg, because the param is used with ##
// m1 shouldn't be expanded in m3's body/expansion, because it's used with ##.
static int m3(m2);
static int m1m2;
