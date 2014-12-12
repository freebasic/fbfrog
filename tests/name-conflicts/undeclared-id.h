// Undeclared identifiers shouldn't be renamed automatically
#define A line
static int i = circle;

// But undeclared identifiers that are declared later should still be renamed
#define getScreen() screen
extern int screen;
