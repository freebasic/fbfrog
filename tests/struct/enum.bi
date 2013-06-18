'' TODO: unknown construct
enum {
 A = 0,
 B, C = (1 << 4),
 D,
 E = CALC(1,2,3),
 F
};


'' TODO: unknown construct
enum E {
 A,

#if 1
'' TODO: unknown construct
B,

#endif
'' TODO: unknown construct
C
};


'' TODO: unknown construct
enum {
 A =
 1
 +
 CALC(2,3)
 ,
 B
};


'' TODO: unknown construct
enum E { A, B, };
enum E{A,B,};
enum E { A, B,
};

