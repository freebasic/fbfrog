#define DEFINED
#undef KNOWNUNDEFINED
#define DEFINED123 123

////////////////////////////////////////////////////////////////////////////////
// atoms

#if 0
	#error
#endif

#if 1
#else
	#error
#endif

#if -1
#else
	#error
#endif

#if 123
#else
	#error
#endif

#if -123
#else
	#error
#endif

#if 0x7FFFFFFF
#else
	#error
#endif

#if 0x80000000
#else
	#error
#endif

#if 0xFFFFFFFF
#else
	#error
#endif

#if 0x7FFFFFFFFFFFFFFF
#else
	#error
#endif

#if 0x8000000000000000
#else
	#error
#endif

#if 0xFFFFFFFFFFFFFFFF
#else
	#error
#endif

#if 0 != 0
	#error
#endif

#if 0 != 1
#else
	#error
#endif

#if 0 != -1
#else
	#error
#endif

#if 00 != 0
	#error
#endif

#if 0100 != 64
	#error
#endif

#if 100 != 100
	#error
#endif

#if 0x0 != 0
	#error
#endif

#if 0x100 != 256
	#error
#endif

#if (123) != 123
	#error
#endif

#if (((123))) != 123
	#error
#endif

#if defined UNDEFINED != 0
	#error
#endif

#if defined KNOWNUNDEFINED != 0
	#error
#endif

#if defined(UNDEFINED) != 0
	#error
#endif

#if defined(KNOWNUNDEFINED) != 0
	#error
#endif

#if UNDEFINED != 0
	#error
#endif

#if KNOWNUNDEFINED != 0
	#error
#endif

#if defined DEFINED != 1
	#error
#endif

#if defined(DEFINED) != 1
	#error
#endif

#ifdef UNDEFINED
	#error
#endif

#ifdef KNOWNUNDEFINED
	#error
#endif

#ifndef UNDEFINED
#else
	#error
#endif

#ifndef KNOWNUNDEFINED
#else
	#error
#endif

#ifdef DEFINED
#else
	#error
#endif

#if 0x7FFFFFFF != 0x7FFFFFFF
	#error
#endif

#if 0x80000000 != 0x80000000
	#error
#endif

#if 0xFFFFFFFF != 0xFFFFFFFF
	#error
#endif

#if 0xFFFFFFFF == -1
	#error
#endif

#if 0x7FFFFFFFFFFFFFFF != 0x7FFFFFFFFFFFFFFF
	#error
#endif

#if 0x7FFFFFFFFFFFFFFF == 0xFFFFFFFF
	#error
#endif

#if 0x8000000000000000 != 0x8000000000000000
	#error
#endif

#if 0x8000000000000000 == 0
	#error
#endif

#if 0xFFFFFFFFFFFFFFFF != 0xFFFFFFFFFFFFFFFF
	#error
#endif

#if 0xFFFFFFFFFFFFFFFF != -1
	#error
#endif

#if -1 != 0xFFFFFFFFFFFFFFFF
	#error
#endif

#if 0xFFFFFFFF == 0xFFFFFFFFFFFFFFFF
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// ! UOP

#if !0 != 1
	#error
#endif

#if !1 != 0
	#error
#endif

#if !-1 != 0
	#error
#endif

#if !0x7FFFFFFF != 0
	#error
#endif

#if !0x80000000 != 0
	#error
#endif

#if !0xFFFFFFFF != 0
	#error
#endif

#if !0x7FFFFFFFFFFFFFFF != 0
	#error
#endif

#if !0x8000000000000000 != 0
	#error
#endif

#if !0xFFFFFFFFFFFFFFFF != 0
	#error
#endif

// Always returns a signed int, even with unsigned operand
#if !0 < -1
	#error
#endif
#if !0u < -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// ~ UOP

#if ~0 != 0xFFFFFFFFFFFFFFFF
	#error
#endif

#if ~1 != 0xFFFFFFFFFFFFFFFE
	#error
#endif

#if ~-1 != 0
	#error
#endif

#if ~0x7FFFFFFF != 0xFFFFFFFF80000000
	#error
#endif

#if ~0x80000000 != 0xFFFFFFFF7FFFFFFF
	#error
#endif

#if ~0xFFFFFFFF != 0xFFFFFFFF00000000
	#error
#endif

#if ~0x7FFFFFFFFFFFFFFF != 0x8000000000000000
	#error
#endif

#if ~0x8000000000000000 != 0x7FFFFFFFFFFFFFFF
	#error
#endif

#if ~0xFFFFFFFFFFFFFFFF != 0
	#error
#endif

// With unsigned operand, the result is unsigned
#if ~0 < -1
	#error
#endif
#if ~0u > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// - UOP

#if -0 != 0
	#error
#endif

#if -1 != 0xFFFFFFFFFFFFFFFF
	#error
#endif

#if - -1 != 1
	#error
#endif

#if -0x7FFFFFFF != 0xFFFFFFFF80000001
	#error
#endif

#if -0x80000000 != 0xFFFFFFFF80000000
	#error
#endif

#if -0xFFFFFFFF != 0xFFFFFFFF00000001
	#error
#endif

#if -0x7FFFFFFFFFFFFFFF != 0x8000000000000001
	#error
#endif

#if -0x8000000000000000 != 0x8000000000000000
	#error
#endif

#if -0xFFFFFFFFFFFFFFFF != 1
	#error
#endif

// With unsigned operand, the result is unsigned
#if -0 < -1
	#error
#endif
#if -0u > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// + UOP

#if +0 != 0
	#error
#endif

#if +1 != 1
	#error
#endif

#if +-1 != -1
	#error
#endif

#if +0x7FFFFFFF != 0x7FFFFFFF
	#error
#endif

#if +0x80000000 != 0x80000000
	#error
#endif

#if +0xFFFFFFFF != 0xFFFFFFFF
	#error
#endif

#if +0x7FFFFFFFFFFFFFFF != 0x7FFFFFFFFFFFFFFF
	#error
#endif

#if +0x8000000000000000 != 0x8000000000000000
	#error
#endif

#if +0xFFFFFFFFFFFFFFFF != 0xFFFFFFFFFFFFFFFF
	#error
#endif

// With unsigned operand, the result is unsigned
#if +0 < -1
	#error
#endif
#if +0u > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// || BOP

#if (0 || 0) != 0
	#error
#endif

#if (1 || 0) != 1
	#error
#endif

#if (0 || 1) != 1
	#error
#endif

#if (1 || 1) != 1
	#error
#endif

#if (-1 || -1) != 1
	#error
#endif

#if (123 || 456) != 1
	#error
#endif

// Short-circuiting
#if (1 || (1 / 0)) != 1
	#error
#endif

#if (0x100000000 || 0) != 1
	#error
#endif

#if (0 || 0x100000000) != 1
	#error
#endif

// || always returns signed int even if operands are unsigned
#if (1 || 1) < -1
	#error
#endif
#if (1u || 1) < -1
	#error
#endif
#if (1 || 1u) < -1
	#error
#endif
#if (1u || 1u) < -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// && BOP

#if (0 && 0) != 0
	#error
#endif

#if (1 && 0) != 0
	#error
#endif

#if (0 && 1) != 0
	#error
#endif

#if (1 && 1) != 1
	#error
#endif

#if (-1 && -1) != 1
	#error
#endif

#if (123 && 456) != 1
	#error
#endif

// Short-circuiting
#if (0 && (1 / 0)) != 0
	#error
#endif

#if (0x100000000 && 1) != 1
	#error
#endif

#if (1 && 0x100000000) != 1
	#error
#endif

// && always returns signed int even if operands are unsigned
#if (1 && 1) < -1
	#error
#endif
#if (1u && 1) < -1
	#error
#endif
#if (1 && 1u) < -1
	#error
#endif
#if (1u && 1u) < -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// | BOP

#if (0 | 0) != 0
	#error
#endif

#if (1 | 0) != 1
	#error
#endif

#if (0 | 1) != 1
	#error
#endif

#if (1 | 1) != 1
	#error
#endif

#if (-1 | -1) != -1
	#error
#endif

#if (0x100000000 | 0) != 0x100000000
	#error
#endif

#if (0 | 0x100000000) != 0x100000000
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 | 1) < -1
	#error
#endif
#if (1u | 1) > -1
	#error
#endif
#if (1 | 1u) > -1
	#error
#endif
#if (1u | 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// ^ BOP

#if (0 ^ 0) != 0
	#error
#endif

#if (1 ^ 0) != 1
	#error
#endif

#if (0 ^ 1) != 1
	#error
#endif

#if (1 ^ 1) != 0
	#error
#endif

#if (-1 ^ -1) != 0
	#error
#endif

#if (0x100000000 ^ 0) != 0x100000000
	#error
#endif

#if (0 ^ 0x100000000) != 0x100000000
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 ^ 0) < -1
	#error
#endif
#if (1u ^ 0) > -1
	#error
#endif
#if (1 ^ 0u) > -1
	#error
#endif
#if (1u ^ 0u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// & BOP

#if (0 & 0) != 0
	#error
#endif

#if (1 & 0) != 0
	#error
#endif

#if (0 & 1) != 0
	#error
#endif

#if (1 & 1) != 1
	#error
#endif

#if (-1 & -1) != -1
	#error
#endif

#if (0x300000000 & 0x100000000) != 0x100000000
	#error
#endif

#if (0x100000000 & 0x300000000) != 0x100000000
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 & 1) < -1
	#error
#endif
#if (1u & 1) > -1
	#error
#endif
#if (1 & 1u) > -1
	#error
#endif
#if (1u & 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// == BOP

#if (0 == 0) != 1
	#error
#endif

#if (1 == 0) != 0
	#error
#endif

#if (0 == 1) != 0
	#error
#endif

#if (1 == 1) != 1
	#error
#endif

#if (-1 == -1) != 1
	#error
#endif

#if (0x100000000 == 0) != 0
	#error
#endif

#if (0 == 0x100000000) != 0
	#error
#endif

// Always returns a signed int, even with unsigned operands
#if (1 == 1) < -1
	#error
#endif
#if (1u == 1) < -1
	#error
#endif
#if (1 == 1u) < -1
	#error
#endif
#if (1u == 1u) < -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// != BOP

#if (0 != 0) != 0
	#error
#endif

#if (1 != 0) != 1
	#error
#endif

#if (0 != 1) != 1
	#error
#endif

#if (1 != 1) != 0
	#error
#endif

#if (-1 != -1) != 0
	#error
#endif

#if (0x100000000 != 0) != 1
	#error
#endif

#if (0 != 0x100000000) != 1
	#error
#endif

// Always returns a signed int, even with unsigned operands
#if (1 != 1) < -1
	#error
#endif
#if (1u != 1) < -1
	#error
#endif
#if (1 != 1u) < -1
	#error
#endif
#if (1u != 1u) < -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// < BOP

#if (0 < 0) != 0
	#error
#endif

#if (1 < 0) != 0
	#error
#endif

#if (0 < 1) != 1
	#error
#endif

#if (1 < 1) != 0
	#error
#endif

#if (-1 < -1) != 0
	#error
#endif

#if (0x100000000 < 0) != 0
	#error
#endif

#if (0 < 0x100000000) != 1
	#error
#endif

// Always returns a signed int, even with unsigned operands
#if (1 < 1) < -1
	#error
#endif
#if (1u < 1) < -1
	#error
#endif
#if (1 < 1u) < -1
	#error
#endif
#if (1u < 1u) < -1
	#error
#endif

// If only one operand is unsigned, the other is promoted to be unsigned too
#if 0 < -1
	#error
#endif
#if -1 < 0u
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// <= BOP

#if (0 <= 0) != 1
	#error
#endif

#if (1 <= 0) != 0
	#error
#endif

#if (0 <= 1) != 1
	#error
#endif

#if (1 <= 1) != 1
	#error
#endif

#if (-1 <= -1) != 1
	#error
#endif

#if (0x100000000 <= 0) != 0
	#error
#endif

#if (0 <= 0x100000000) != 1
	#error
#endif

// Always returns a signed int, even with unsigned operands
#if (1 <= 1) < -1
	#error
#endif
#if (1u <= 1) < -1
	#error
#endif
#if (1 <= 1u) < -1
	#error
#endif
#if (1u <= 1u) < -1
	#error
#endif

// If only one operand is unsigned, the other is promoted to be unsigned too
#if 0 <= -1
	#error
#endif
#if -1 <= 0u
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// > BOP

#if (0 > 0) != 0
	#error
#endif

#if (1 > 0) != 1
	#error
#endif

#if (0 > 1) != 0
	#error
#endif

#if (1 > 1) != 0
	#error
#endif

#if (-1 > -1) != 0
	#error
#endif

#if (0x100000000 > 0) != 1
	#error
#endif

#if (0 > 0x100000000) != 0
	#error
#endif

// Always returns a signed int, even with unsigned operands
#if (1 > 1) < -1
	#error
#endif
#if (1u > 1) < -1
	#error
#endif
#if (1 > 1u) < -1
	#error
#endif
#if (1u > 1u) < -1
	#error
#endif

// If only one operand is unsigned, the other is promoted to be unsigned too
#if -1 > 0
	#error
#endif
#if 0u > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// >= BOP

#if (0 >= 0) != 1
	#error
#endif

#if (1 >= 0) != 1
	#error
#endif

#if (0 >= 1) != 0
	#error
#endif

#if (1 >= 1) != 1
	#error
#endif

#if (-1 >= -1) != 1
	#error
#endif

#if (0x100000000 >= 0) != 1
	#error
#endif

#if (0 >= 0x100000000) != 0
	#error
#endif

// Always returns a signed int, even with unsigned operands
#if (1 >= 1) < -1
	#error
#endif
#if (1u >= 1) < -1
	#error
#endif
#if (1 >= 1u) < -1
	#error
#endif
#if (1u >= 1u) < -1
	#error
#endif

// If only one operand is unsigned, the other is promoted to be unsigned too
#if -1 >= 0
	#error
#endif
#if 0u >= -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// << BOP

#if (0 << 0) != 0
	#error
#endif

#if (1 << 0) != 1
	#error
#endif

#if (0 << 1) != 0
	#error
#endif

#if (1 << 1) != 2
	#error
#endif

#if (0x100000000 << 1) != 0x200000000
	#error
#endif

#if (0x80000000 << 1) != 0x100000000
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 << 1) < -1
	#error
#endif
#if (1u << 1) > -1
	#error
#endif
#if (1 << 1u) > -1
	#error
#endif
#if (1u << 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// >> BOP

#if (0 >> 0) != 0
	#error
#endif

#if (1 >> 0) != 1
	#error
#endif

#if (0 >> 1) != 0
	#error
#endif

#if (1 >> 1) != 0
	#error
#endif

#if (0x200000000 >> 1) != 0x100000000
	#error
#endif

#if (0x100000000 >> 1) != 0x80000000
	#error
#endif

// With unsigned operands, the result is unsigned
#if (4 >> 1) < -1
	#error
#endif
#if (4u >> 1) > -1
	#error
#endif
#if (4 >> 1u) > -1
	#error
#endif
#if (4u >> 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// + BOP

#if (0 + 0) != 0
	#error
#endif

#if (1 + 0) != 1
	#error
#endif

#if (0 + 1) != 1
	#error
#endif

#if (1 + 1) != 2
	#error
#endif

#if (-1 + -1) != -2
	#error
#endif

#if (0x100000000 + 0) != 0x100000000
	#error
#endif

#if (0 + 0x100000000) != 0x100000000
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 + 1) < -1
	#error
#endif
#if (1u + 1) > -1
	#error
#endif
#if (1 + 1u) > -1
	#error
#endif
#if (1u + 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// - BOP

#if (0 - 0) != 0
	#error
#endif

#if (1 - 0) != 1
	#error
#endif

#if (0 - 1) != -1
	#error
#endif

#if (1 - 1) != 0
	#error
#endif

#if (-1 - -1) != 0
	#error
#endif

#if (0x100000000 - 0) != 0x100000000
	#error
#endif

#if (0 - 0x100000000) != -0x100000000
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 - 1) < -1
	#error
#endif
#if (1u - 1) > -1
	#error
#endif
#if (1 - 1u) > -1
	#error
#endif
#if (1u - 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// * BOP

#if (0 * 0) != 0
	#error
#endif

#if (1 * 0) != 0
	#error
#endif

#if (0 * 1) != 0
	#error
#endif

#if (1 * 1) != 1
	#error
#endif

#if (-1 * -1) != 1
	#error
#endif

#if (100 * 5) != 500
	#error
#endif

#if (0x100000000 * 1) != 0x100000000
	#error
#endif

#if (1 * 0x100000000) != 0x100000000
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 * 1) < -1
	#error
#endif
#if (1u * 1) > -1
	#error
#endif
#if (1 * 1u) > -1
	#error
#endif
#if (1u * 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// / BOP

#if (0 / 1) != 0
	#error
#endif

#if (1 / 1) != 1
	#error
#endif

#if (-1 / -1) != 1
	#error
#endif

#if (100 / 5) != 20
	#error
#endif

#if (0x100000000 / 1) != 0x100000000
	#error
#endif

#if (0x100000000 / 0x100000000) != 1
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 / 1) < -1
	#error
#endif
#if (1u / 1) > -1
	#error
#endif
#if (1 / 1u) > -1
	#error
#endif
#if (1u / 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// % BOP

#if (0 % 1) != 0
	#error
#endif

#if (1 % 1) != 0
	#error
#endif

#if (-1 % -1) != 0
	#error
#endif

#if (100 % 5) != 0
	#error
#endif

#if (11 % 3) != 2
	#error
#endif

#if (0x100000000 % 1) != 0
	#error
#endif

#if (0x100000000 % 0x100000000) != 0
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 % 1) < -1
	#error
#endif
#if (1u % 1) > -1
	#error
#endif
#if (1 % 1u) > -1
	#error
#endif
#if (1u % 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////
// iif

// Short-circuiting
#if (1 ? 123 : (1 / 0)) != 123
	#error
#endif

// Short-circuiting
#if (0 ? (1 / 0) : 123) != 123
	#error
#endif

#if (-1 ? 3 : 4) != 3
	#error
#endif

// With unsigned operands, the result is unsigned
#if (1 ? 1 : 1) < -1
	#error
#endif
#if (1 ? 1u : 1) > -1
	#error
#endif
#if (1 ? 1 : 1u) > -1
	#error
#endif
#if (1 ? 1u : 1u) > -1
	#error
#endif

////////////////////////////////////////////////////////////////////////////////

#if (defined DEFINED && DEFINED123) == 0
	#error
#endif

#if (defined DEFINED123 && DEFINED123 == 123) == 0
	#error
#endif

#if defined UNDEFINED && UNDEFINED == 123
	#error
#endif
