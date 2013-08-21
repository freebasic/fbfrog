extern __defined as long
#if defined( foo_1 ) = 1
#endif
#if defined( foo_0 ) = 0
#endif

extern __iif as long
#if defined( foo_r ) = r
#endif
#if defined( foo_l ) = l
#endif
#if defined( foo_l ) = l
#endif

extern __andalso as long
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif

extern __orelse as long
#if defined( foo_0 ) = 0
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif

extern __bitwise as long
#if defined( foo_0xFF ) = 255
#endif
#if defined( foo_0x0F ) = 15
#endif
#if defined( foo_0xF0 ) = 240
#endif

extern __shl as long
#if defined( foo_1 ) = 1
#endif
#if defined( foo_2 ) = 2
#endif
#if defined( foo_4 ) = 4
#endif
#if defined( foo_8 ) = 8
#endif
#if defined( foo_0 ) = 0
#endif

extern __shr as long
#if defined( foo_1 ) = 1
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_2 ) = 2
#endif
#if defined( foo_0 ) = 0
#endif

extern __relational as long
#if defined( foo_1 ) = 1
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_0 ) = 0
#endif

extern __math as long
#if defined( foo_3 ) = 3
#endif
#if defined( foo_3 ) = 3
#endif
#if defined( foo_4 ) = 4
#endif
#if defined( foo_3 ) = 3
#endif
#if defined( foo_2 ) = 2
#endif

extern __nop_orelse as long
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_1 ) = 1
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif

extern __nop_andalso as long
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_0 ) = 0
#endif

extern __nop_bitwise as long
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif

extern __nop_shlshr as long
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif

extern __nop_math as long
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_0 ) = 0
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
#if defined( foo_minus_defined_bar ) = (-defined( bar ))
#endif
#if defined( foo_defined_bar ) = defined( bar )
#endif
