// Sometimes scope blocks don't even have ';' semi-colons
#define M1 { if (1); }
#define M2 { if (1) { } }
#define M3 { do ; while (1); }
#define M4 { do { } while (1); }
#define M5 { while (1); }
#define M6 { while (1) { } }

// 1. This macro body is a statement, and it's not a scope block, so fbfrog will
//    wrap it in a scope block
// 2. Highlevel transformations turn the do/while(0) into a scope block
// 3. Now there are two scope blocks that should both be removed since the
//    inner if block is good enough.
#define A1 do { if (1) f(1); } while (0)
