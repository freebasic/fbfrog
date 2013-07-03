static int a[1];
static int a[10];
static int a[2][2];
static int a[2][2][2];
static int a[2][3][4][5][6];
static void (*p[4])(void);
static void (*p[2][3])(void);

int a[10];
extern int a[10];

static int a[10+10+10*2+1];
