#define KNOWNDEFINED1
#undef KNOWNUNDEFINED1

// defined()
#if defined FOO
#if !defined FOO
#ifdef FOO
#ifndef FOO
#if defined KNOWNDEFINED1
#if !defined KNOWNDEFINED1
#ifdef KNOWNDEFINED1
#ifndef KNOWNDEFINED1
#if foo_1 == (defined KNOWNDEFINED1)
#if foo_0 == (defined KNOWNUNDEFINED1)

// iif
#if foo_r == (0 ? l : r)
#if foo_l == (1 ? l : r)
#if foo_l == (-1 ? l : r)

// andalso
#if foo_0 == (0 && 0)
#if foo_0 == (0 && 1)
#if foo_0 == (0 && 2)
#if foo_0 == (1 && 0)
#if foo_0 == (2 && 0)
#if foo_1 == (1 && 1)
#if foo_1 == (2 && 2)

// orelse
#if foo_0 == (0 || 0)
#if foo_1 == (0 || 1)
#if foo_1 == (0 || 2)
#if foo_1 == (1 || 0)
#if foo_1 == (2 || 0)
#if foo_1 == (1 || 1)
#if foo_1 == (2 || 2)

// bitwise
#if foo_0xFF == (0xF0 | 0x0F)
#if foo_0x0F == (0xFF & 0x0F)
#if foo_0xF0 == (0xFF ^ 0x0F)

// shl
#if foo_1 == (1 << 0)
#if foo_2 == (1 << 1)
#if foo_4 == (2 << 1)
#if foo_8 == (2 << 2)
#if foo_0 == (0 << 1)

// shr
#if foo_1 == (1 >> 0)
#if foo_0 == (1 >> 1)
#if foo_1 == (2 >> 1)
#if foo_2 == (8 >> 2)
#if foo_0 == (0 >> 1)

// relational
#if foo_1 == (0 == 0)
#if foo_0 == (1 == 2)
#if foo_0 == (0 != 0)
#if foo_1 == (1 != 2)
#if foo_0 == (0 < 0)
#if foo_1 == (1 < 2)
#if foo_0 == (2 < 1)
#if foo_1 == (0 <= 0)
#if foo_1 == (1 <= 2)
#if foo_0 == (2 <= 1)

// math
#if foo_3 == (2 + 1)
#if foo_3 == (6 - 3)
#if foo_4 == (2 * 2)
#if foo_3 == (15 / 5)
#if foo_2 == (17 % 5)
#if foo_0 == (0 / 1)
#if foo_0 == (0 % 1)

// iif nop
#if foo_0 == (defined x ? 0 : 0)
#if foo_123 == (defined x ? 123 : 123)
#if foo_defined_y == (defined x ? defined y : defined y)
#if foo_defined_y_1_2 == (defined x ? (defined y ? 1 : 2) : (defined y ? 1 : 2))

// orelse nop
#if foo_1 == (1 || defined bar)
#if foo_1 == (2 || defined bar)
#if foo_defined_bar == (0 || defined bar)
#if foo_1 == (defined bar || 1)
#if foo_1 == (defined bar || 2)
#if foo_defined_bar == (defined bar || 0)

// andalso nop
#if foo_defined_bar == (1 && defined bar)
#if foo_defined_bar == (2 && defined bar)
#if foo_0 == (0 && defined bar)
#if foo_defined_bar == (defined bar && 1)
#if foo_defined_bar == (defined bar && 2)
#if foo_0 == (defined bar && 0)

// bitwise nop
#if foo_defined_bar == (0 | defined bar)
#if foo_defined_bar == (defined bar | 0)
#if foo_0 == (0 & defined bar)
#if foo_0 == (defined bar & 0)

// shl/shr nop
#if foo_0 == (0 << defined bar)
#if foo_0 == (0 >> defined bar)
#if foo_defined_bar == (defined bar << 0)
#if foo_defined_bar == (defined bar >> 0)

// math nop
#if foo_defined_bar == (0 + defined bar)
#if foo_defined_bar == (defined bar + 0)
#if foo_0 == (0 / defined bar)
#if foo_0 == (0 % defined bar)
#if foo_0 == (0 * defined bar)
#if foo_defined_bar == (1 * defined bar)
#if foo_0 == (defined bar * 0)
#if foo_defined_bar == (defined bar * 1)
#if foo_minus_defined_bar == (0 - defined bar)
#if foo_defined_bar == (defined bar - 0)

// math error: divison by zero
#if 1 / 0
#if 0 / 0
#if 1 % 0
#if 0 % 0
