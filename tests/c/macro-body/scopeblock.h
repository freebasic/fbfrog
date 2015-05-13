// Sometimes scope blocks don't even have ';' semi-colons
#define M1 { if (1); }
#define M2 { if (1) { } }
#define M3 { do ; while (1); }
#define M4 { do { } while (1); }
#define M5 { while (1); }
#define M6 { while (1) { } }
