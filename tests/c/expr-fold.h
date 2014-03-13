// @fbfrog -whitespace -nonamefixup

enum E {
	// UOPs
	_1 = !0,
	_0 = !(-1),
	_minus_1 = ~0,
	_0 = ~(-1),
	_0 = -0,
	_0 = +0,

	// iif
	r = 0 ? l : r,
	l = 1 ? l : r,
	l = -1 ? l : r,

	// andalso
	_0 = 0 && 0,
	_0 = 0 && 1,
	_0 = 0 && 2,
	_0 = 1 && 0,
	_0 = 2 && 0,
	_1 = 1 && 1,
	_1 = 2 && 2,

	// orelse
	_0 = 0 || 0,
	_1 = 0 || 1,
	_1 = 0 || 2,
	_1 = 1 || 0,
	_1 = 2 || 0,
	_1 = 1 || 1,
	_1 = 2 || 2,

	// bitwise
	_0xFF = 0xF0 | 0x0F,
	_0x0F = 0xFF & 0x0F,
	_0xF0 = 0xFF ^ 0x0F,

	// shl
	_1 = 1 << 0,
	_2 = 1 << 1,
	_4 = 2 << 1,
	_8 = 2 << 2,
	_0 = 0 << 1,

	// shr
	_1 = 1 >> 0,
	_0 = 1 >> 1,
	_1 = 2 >> 1,
	_2 = 8 >> 2,
	_0 = 0 >> 1,

	// relational
	_1 = 0 == 0,
	_0 = 1 == 2,
	_0 = 0 != 0,
	_1 = 1 != 2,
	_0 = 0 <  0,
	_1 = 1 <  2,
	_0 = 2 <  1,
	_1 = 0 <= 0,
	_1 = 1 <= 2,
	_0 = 2 <= 1,
	_0 = 0 >  0,
	_1 = 2 >  1,
	_0 = 1 >  2,
	_1 = 0 >= 0,
	_1 = 2 >= 1,
	_0 = 1 >= 2,

	// math
	_3 = 2 + 1,
	_3 = 6 - 3,
	_4 = 2 * 2,
	_3 = 15 / 5,
	_2 = 17 % 5,
	_0 = 0 / 1,
	_0 = 0 % 1,

	// iif nop
	_0     = x ? 0 : 0,
	_123   = x ? 123 : 123,
	_y     = x ? y : y,
	_iif_y_1_2 = x ? (y ? 1 : 2) : (y ? 1 : 2),

	// orelse nop
	_1 = 1 || x,
	_1 = 2 || x,
	_x = 0 || x,
	_1 = x || 1,
	_1 = x || 2,
	_x = x || 0,

	// andalso nop
	_x = 1 && x,
	_x = 2 && x,
	_0 = 0 && x,
	_x = x && 1,
	_x = x && 2,
	_0 = x && 0,

	// bitwise nop
	_x = 0 | x,
	_x = x | 0,
	_0 = 0 & x,
	_0 = x & 0,

	// shl/shr nop
	_0 = 0 << x,
	_0 = 0 >> x,
	_x = x << 0,
	_x = x >> 0,

	// math nop
	_x = 0 + x,
	_x = x + 0,
	_0 = 0 / x,
	_0 = 0 % x,
	_0 = 0 * x,
	_x = 1 * x,
	_0 = x * 0,
	_x = x * 1,
	_minus_x = 0 - x,
	_x = x - 0,

	// math error: divison by zero, cannot be folded
	A = 1 / 0,
	A = 0 / 0,
	A = 1 % 0,
	A = 0 % 0,

	// Const folding should preserve oct/hex flags, for some ops
	// hex
	_dec = !0x0,
	_dec = !(0xFFFFFFFF),
	_hex = ~0x0,
	_hex = ~(0xFFFFFFFF),
	_hex = -0x0,
	_hex = +0x0,

	_dec = 0x0 && 0x0,
	_dec = 0x0 || 0x0,
	_hex = 0x0 |  0x0,
	_hex = 0x0 &  0x0,
	_hex = 0x0 ^  0x0,
	_hex = 0x0 << 0x0,
	_hex = 0x0 >> 0x0,
	_dec = 0x0 == 0x0,
	_dec = 0x0 != 0x0,
	_dec = 0x0 <  0x0,
	_dec = 0x0 >  0x0,
	_dec = 0x0 <= 0x0,
	_dec = 0x0 >= 0x0,
	_hex = 0x0 +  0x0,
	_hex = 0x0 -  0x0,
	_hex = 0x0 *  0x0,
	_hex = 0x0 /  0x1,
	_hex = 0x0 %  0x1,

	// oct (using 00 instead of 0 because fbfrog treats 0 as decimal)
	_dec = !00,
	_dec = !(037777777777),
	_oct = ~00,
	_oct = ~(037777777777),
	_oct = -00,
	_oct = +00,

	_dec = 00 && 00,
	_dec = 00 || 00,
	_hex = 00 |  00,
	_hex = 00 &  00,
	_hex = 00 ^  00,
	_hex = 00 << 00,
	_hex = 00 >> 00,
	_dec = 00 == 00,
	_dec = 00 != 00,
	_dec = 00 <  00,
	_dec = 00 >  00,
	_dec = 00 <= 00,
	_dec = 00 >= 00,
	_hex = 00 +  00,
	_hex = 00 -  00,
	_hex = 00 *  00,
	_hex = 00 /  01,
	_hex = 00 %  01,

	// mixed oct/hex
	_hex = 0xFF & 0377,
	_hex = 0377 & 0xFF,

	// nops
	_hex = 0x0 % x,
	_hex = 0x0 / x,
	_hex = 0x0 << x,
	_hex = 0x0 >> x,
	_hex = x & 0x0,
	_hex = x | 0xFFFFFFFF,
	_hex = x * 0x0,
	_hex = 0x0 & x,
	_hex = 0xFFFFFFFF | x,
	_hex = 0x0 * x,
};
