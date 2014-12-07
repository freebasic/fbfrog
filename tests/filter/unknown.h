// @ignore

foo;

#define A 123    // immediate, ok
#define B 123+   // immediate, error
#define C X      // delayed, ok
#define D Y+     // delayed, error
