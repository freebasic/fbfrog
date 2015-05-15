#define a #define foo

// regression test: the "else" is unexpected and will be skipped; the "#expr"
// shouldn't be treated as CPP directive by the skipping...
#define b(expr) { else #expr }
