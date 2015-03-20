// @fbfrog -dropmacrobodyscopes

#define A { } // this is currently treated as struct initializer
#define B { ; } // empty scope block - should be removed
#define C { f1(0); } // just 1 statement - should remove scope block here
#define D { f1(0); f2(0); } // multiple statements - should keep scope block here
