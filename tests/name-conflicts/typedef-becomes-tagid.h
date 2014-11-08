// The anon struct will be given the typedef's name; the typedef is removed.
// This should not prevent any references to that id from being adjusted in case
// it needs to be renamed.

typedef struct { int a; } SCREEN;
extern SCREEN x;
