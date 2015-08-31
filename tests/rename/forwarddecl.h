// @fbfrog -renamedefine MY_INTEGER MY_INTEGER_

// If adding a forward declaration for a typedef that was renamed, the forward
// declaration should appear in the rename list, instead of the typedef.
void f(MY_INTEGER *p);
#define MY_INTEGER long long
typedef MY_INTEGER my_Integer;
