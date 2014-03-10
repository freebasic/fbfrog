#define EXPANDME1 x1
#define EXPANDME2 x2
#define EXPANDME3(x) EXPANDME1##x

// EXPANDME2 shouldn't be expanded in the arg, because the param is used with ##
// EXPANDME1 shouldn't be expanded in EXPANDME3's body/expansion, because it's
// used with ##.
static int EXPANDME3(EXPANDME2);
static int EXPANDME1EXPANDME2;
