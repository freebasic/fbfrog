// Initializer parsing cannot easily distinguish between struct & array initializers,
// which look the same in C but have different syntax in FB.

struct Vector2A {
	int x, y;
};

static struct Vector2A a[4] = {
	{ 0, 0 },
	{ 1, 0 },
	{ 0, 1 },
	{ 1, 1 }
};

struct Vector2B {
	int coord[2];
};

static struct Vector2B b[4] = {
	{ 0, 0 },
	{ 1, 0 },
	{ 0, 1 },
	{ 1, 1 }
};
