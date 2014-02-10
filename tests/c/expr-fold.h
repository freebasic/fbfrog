enum E {
	// iif
	A = foo_r == (0 ? l : r),
	A = foo_l == (1 ? l : r),
	A = foo_l == (-1 ? l : r),

	// andalso
	A = foo_0 == (0 && 0),
	A = foo_0 == (0 && 1),
	A = foo_0 == (0 && 2),
	A = foo_0 == (1 && 0),
	A = foo_0 == (2 && 0),
	A = foo_1 == (1 && 1),
	A = foo_1 == (2 && 2),

	// orelse
	A = foo_0 == (0 || 0),
	A = foo_1 == (0 || 1),
	A = foo_1 == (0 || 2),
	A = foo_1 == (1 || 0),
	A = foo_1 == (2 || 0),
	A = foo_1 == (1 || 1),
	A = foo_1 == (2 || 2),

	// bitwise
	A = foo_0xFF == (0xF0 | 0x0F),
	A = foo_0x0F == (0xFF & 0x0F),
	A = foo_0xF0 == (0xFF ^ 0x0F),

	// shl
	A = foo_1 == (1 << 0),
	A = foo_2 == (1 << 1),
	A = foo_4 == (2 << 1),
	A = foo_8 == (2 << 2),
	A = foo_0 == (0 << 1),

	// shr
	A = foo_1 == (1 >> 0),
	A = foo_0 == (1 >> 1),
	A = foo_1 == (2 >> 1),
	A = foo_2 == (8 >> 2),
	A = foo_0 == (0 >> 1),

	// relational
	A = foo_1 == (0 == 0),
	A = foo_0 == (1 == 2),
	A = foo_0 == (0 != 0),
	A = foo_1 == (1 != 2),
	A = foo_0 == (0 < 0),
	A = foo_1 == (1 < 2),
	A = foo_0 == (2 < 1),
	A = foo_1 == (0 <= 0),
	A = foo_1 == (1 <= 2),
	A = foo_0 == (2 <= 1),

	// math
	A = foo_3 == (2 + 1),
	A = foo_3 == (6 - 3),
	A = foo_4 == (2 * 2),
	A = foo_3 == (15 / 5),
	A = foo_2 == (17 % 5),
	A = foo_0 == (0 / 1),
	A = foo_0 == (0 % 1),

	// iif nop
	A = foo_0 == (x ? 0 : 0),
	A = foo_123 == (x ? 123 : 123),
	A = foo_y == (x ? y : y),
	A = foo_y_1_2 == (x ? (y ? 1 : 2) : (y ? 1 : 2)),

	// orelse nop
	A = foo_1 == (1 || x),
	A = foo_1 == (2 || x),
	A = foo_x == (0 || x),
	A = foo_1 == (x || 1),
	A = foo_1 == (x || 2),
	A = foo_x == (x || 0),

	// andalso nop
	A = foo_x == (1 && x),
	A = foo_x == (2 && x),
	A = foo_0 == (0 && x),
	A = foo_x == (x && 1),
	A = foo_x == (x && 2),
	A = foo_0 == (x && 0),

	// bitwise nop
	A = foo_x == (0 | x),
	A = foo_x == (x | 0),
	A = foo_0 == (0 & x),
	A = foo_0 == (x & 0),

	// shl/shr nop
	A = foo_0 == (0 << x),
	A = foo_0 == (0 >> x),
	A = foo_x == (x << 0),
	A = foo_x == (x >> 0),

	// math nop
	A = foo_x == (0 + x),
	A = foo_x == (x + 0),
	A = foo_0 == (0 / x),
	A = foo_0 == (0 % x),
	A = foo_0 == (0 * x),
	A = foo_x == (1 * x),
	A = foo_0 == (x * 0),
	A = foo_x == (x * 1),
	A = foo_minus_x == (0 - x),
	A = foo_x == (x - 0),

	// math error: divison by zero
	A = 1 / 0,
	A = 0 / 0,
	A = 1 % 0,
	A = 0 % 0
};
