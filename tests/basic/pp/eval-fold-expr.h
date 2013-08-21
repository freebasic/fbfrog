extern int __defined;
#if defined foo_1 == (defined KNOWNDEFINED1)
#endif
#if defined foo_0 == (defined KNOWNUNDEFINED1)
#endif

extern int __iif;
#if defined foo_r == (0 ? l : r)
#endif
#if defined foo_l == (1 ? l : r)
#endif
#if defined foo_l == (-1 ? l : r)
#endif

extern int __andalso;
#if defined foo_0 == (0 && 0)
#endif
#if defined foo_0 == (0 && 1)
#endif
#if defined foo_0 == (0 && 2)
#endif
#if defined foo_0 == (1 && 0)
#endif
#if defined foo_0 == (2 && 0)
#endif
#if defined foo_1 == (1 && 1)
#endif
#if defined foo_1 == (2 && 2)
#endif

extern int __orelse;
#if defined foo_0 == (0 || 0)
#endif
#if defined foo_1 == (0 || 1)
#endif
#if defined foo_1 == (0 || 2)
#endif
#if defined foo_1 == (1 || 0)
#endif
#if defined foo_1 == (2 || 0)
#endif
#if defined foo_1 == (1 || 1)
#endif
#if defined foo_1 == (2 || 2)
#endif

extern int __bitwise;
#if defined foo_0xFF == (0xF0 | 0x0F)
#endif
#if defined foo_0x0F == (0xFF & 0x0F)
#endif
#if defined foo_0xF0 == (0xFF ^ 0x0F)
#endif

extern int __shl;
#if defined foo_1 == (1 << 0)
#endif
#if defined foo_2 == (1 << 1)
#endif
#if defined foo_4 == (2 << 1)
#endif
#if defined foo_8 == (2 << 2)
#endif
#if defined foo_0 == (0 << 1)
#endif

extern int __shr;
#if defined foo_1 == (1 >> 0)
#endif
#if defined foo_0 == (1 >> 1)
#endif
#if defined foo_1 == (2 >> 1)
#endif
#if defined foo_2 == (8 >> 2)
#endif
#if defined foo_0 == (0 >> 1)
#endif

extern int __relational;
#if defined foo_1 == (0 == 0)
#endif
#if defined foo_0 == (1 == 2)
#endif
#if defined foo_0 == (0 != 0)
#endif
#if defined foo_1 == (1 != 2)
#endif
#if defined foo_0 == (0 < 0)
#endif
#if defined foo_1 == (1 < 2)
#endif
#if defined foo_0 == (2 < 1)
#endif
#if defined foo_1 == (0 <= 0)
#endif
#if defined foo_1 == (1 <= 2)
#endif
#if defined foo_0 == (2 <= 1)
#endif

extern int __math;
#if defined foo_3 == (2 + 1)
#endif
#if defined foo_3 == (6 - 3)
#endif
#if defined foo_4 == (2 * 2)
#endif
#if defined foo_3 == (15 / 5)
#endif
#if defined foo_2 == (17 % 5)
#endif

extern int __nop_orelse;
#if defined foo_1 == (1 || defined bar)
#endif
#if defined foo_1 == (2 || defined bar)
#endif
#if defined foo_defined_bar == (0 || defined bar)
#endif
#if defined foo_1 == (defined bar || 1)
#endif
#if defined foo_1 == (defined bar || 2)
#endif
#if defined foo_defined_bar == (defined bar || 0)
#endif

extern int __nop_andalso;
#if defined foo_defined_bar == (1 && defined bar)
#endif
#if defined foo_defined_bar == (2 && defined bar)
#endif
#if defined foo_0 == (0 && defined bar)
#endif
#if defined foo_defined_bar == (defined bar && 1)
#endif
#if defined foo_defined_bar == (defined bar && 2)
#endif
#if defined foo_0 == (defined bar && 0)
#endif

extern int __nop_bitwise;
#if defined foo_defined_bar == (0 | defined bar)
#endif
#if defined foo_defined_bar == (defined bar | 0)
#endif
#if defined foo_0 == (0 & defined bar)
#endif
#if defined foo_0 == (defined bar & 0)
#endif

extern int __nop_shlshr;
#if defined foo_0 == (0 << defined bar)
#endif
#if defined foo_0 == (0 >> defined bar)
#endif
#if defined foo_defined_bar == (defined bar << 0)
#endif
#if defined foo_defined_bar == (defined bar >> 0)
#endif

extern int __nop_math;
#if defined foo_defined_bar == (0 + defined bar)
#endif
#if defined foo_defined_bar == (defined bar + 0)
#endif
#if defined foo_0 == (0 / defined bar)
#endif
#if defined foo_0 == (0 % defined bar)
#endif
#if defined foo_0 == (0 * defined bar)
#endif
#if defined foo_defined_bar == (1 * defined bar)
#endif
#if defined foo_0 == (defined bar * 0)
#endif
#if defined foo_defined_bar == (defined bar * 1)
#endif
#if defined foo_minus_defined_bar == (0 - defined bar)
#endif
#if defined foo_defined_bar == (defined bar - 0)
#endif
