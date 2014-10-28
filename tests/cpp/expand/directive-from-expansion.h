// @fbfrog -v -removedefine m

// CPP doesn't support directives coming from macro expansions
#define m #define foo
m
