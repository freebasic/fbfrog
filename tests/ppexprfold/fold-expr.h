// defined()
#if defined foo_1 == (defined KNOWNDEFINED1)
#if defined foo_0 == (defined KNOWNUNDEFINED1)

// iif
#if defined foo_r == (0 ? l : r)
#if defined foo_l == (1 ? l : r)
#if defined foo_l == (-1 ? l : r)

// andalso
#if defined foo_0 == (0 && 0)
#if defined foo_0 == (0 && 1)
#if defined foo_0 == (0 && 2)
#if defined foo_0 == (1 && 0)
#if defined foo_0 == (2 && 0)
#if defined foo_1 == (1 && 1)
#if defined foo_1 == (2 && 2)

// orelse
#if defined foo_0 == (0 || 0)
#if defined foo_1 == (0 || 1)
#if defined foo_1 == (0 || 2)
#if defined foo_1 == (1 || 0)
#if defined foo_1 == (2 || 0)
#if defined foo_1 == (1 || 1)
#if defined foo_1 == (2 || 2)

// bitwise
#if defined foo_0xFF == (0xF0 | 0x0F)
#if defined foo_0x0F == (0xFF & 0x0F)
#if defined foo_0xF0 == (0xFF ^ 0x0F)

// shl
#if defined foo_1 == (1 << 0)
#if defined foo_2 == (1 << 1)
#if defined foo_4 == (2 << 1)
#if defined foo_8 == (2 << 2)
#if defined foo_0 == (0 << 1)

// shr
#if defined foo_1 == (1 >> 0)
#if defined foo_0 == (1 >> 1)
#if defined foo_1 == (2 >> 1)
#if defined foo_2 == (8 >> 2)
#if defined foo_0 == (0 >> 1)

// relational
#if defined foo_1 == (0 == 0)
#if defined foo_0 == (1 == 2)
#if defined foo_0 == (0 != 0)
#if defined foo_1 == (1 != 2)
#if defined foo_0 == (0 < 0)
#if defined foo_1 == (1 < 2)
#if defined foo_0 == (2 < 1)
#if defined foo_1 == (0 <= 0)
#if defined foo_1 == (1 <= 2)
#if defined foo_0 == (2 <= 1)

// math
#if defined foo_3 == (2 + 1)
#if defined foo_3 == (6 - 3)
#if defined foo_4 == (2 * 2)
#if defined foo_3 == (15 / 5)
#if defined foo_2 == (17 % 5)
#if defined foo_0 == (0 / 1)
#if defined foo_0 == (0 % 1)

// orelse nop
#if defined foo_1 == (1 || defined bar)
#if defined foo_1 == (2 || defined bar)
#if defined foo_defined_bar == (0 || defined bar)
#if defined foo_1 == (defined bar || 1)
#if defined foo_1 == (defined bar || 2)
#if defined foo_defined_bar == (defined bar || 0)

// andalso nop
#if defined foo_defined_bar == (1 && defined bar)
#if defined foo_defined_bar == (2 && defined bar)
#if defined foo_0 == (0 && defined bar)
#if defined foo_defined_bar == (defined bar && 1)
#if defined foo_defined_bar == (defined bar && 2)
#if defined foo_0 == (defined bar && 0)

// bitwise nop
#if defined foo_defined_bar == (0 | defined bar)
#if defined foo_defined_bar == (defined bar | 0)
#if defined foo_0 == (0 & defined bar)
#if defined foo_0 == (defined bar & 0)

// shl/shr nop
#if defined foo_0 == (0 << defined bar)
#if defined foo_0 == (0 >> defined bar)
#if defined foo_defined_bar == (defined bar << 0)
#if defined foo_defined_bar == (defined bar >> 0)

// math nop
#if defined foo_defined_bar == (0 + defined bar)
#if defined foo_defined_bar == (defined bar + 0)
#if defined foo_0 == (0 / defined bar)
#if defined foo_0 == (0 % defined bar)
#if defined foo_0 == (0 * defined bar)
#if defined foo_defined_bar == (1 * defined bar)
#if defined foo_0 == (defined bar * 0)
#if defined foo_defined_bar == (defined bar * 1)
#if defined foo_minus_defined_bar == (0 - defined bar)
#if defined foo_defined_bar == (defined bar - 0)

// math error: divison by zero
#if 1 / 0
#if 0 / 0
#if 1 % 0
#if 0 % 0
