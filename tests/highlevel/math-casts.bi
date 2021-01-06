#pragma once

extern "C"

type E as long
enum
	ENUMCONST1 = 0
end enum

const A58 = culng(0u - 100u)
const A59 = culng(0u - 100)
const A60 = 0 - 100
const A61 = culng(0 - 100u)
const A62 = culng(0u - 100u)
const A70 = culng(A58 - 1u)
const A71 = culng(A58 - 1)
const A72 = culng(culng(A58) - 1u)
const A73 = culng(culng(A58) - 1)
const A74 = culng(ENUMCONST1 - 1u)
const A75 = ENUMCONST1 - 1
const A76 = culng(culng(ENUMCONST1) - 1u)
const A77 = culng(culng(ENUMCONST1) - 1)
#define A78 (undefined - 1u)
#define A79 (undefined - 1)
#define A80 culng(culng(undefined) - 1u)
#define A81 culng(culng(undefined) - 1)
#define A82 culng(clng(undefined) - 1u)
#define A83 (clng(undefined) - 1)

#ifdef __FB_WIN32__
	#define A84 undefined
	#define A85 A84
#else
	const A84 = -1
	const A85 = A84
#endif

const B00 = culng(0u - 100u)
const B01 = B00
#define B11 culng(B10 - 1)
const B10 = culng(0u - 100u)
#define B22 culng(B21 - 1)
#define B21 culng(B20 - 1)
const B20 = culng(0u - 100u)
#define B31 culng(B30 - 1)
#define B32 culng(B31 - 1)
const B30 = culng(0u - 100u)
type UINT as ulong
const C00_MAX_UINT32 = cuint(culng(not 0u))
const C01_MAX_UINT32 = cuint(culng(not culng(0)))
const C02_MAX_UINT32 = cuint(not cast(UINT, 0))
const C03_MAX_UINTPTR = cuint(not 0)
dim shared C04(0 to cuint(not cast(UINT, 0)) - 1) as uinteger = {cuint(not cast(UINT, 0))}
declare sub C05(byval param as uinteger = cuint(not cast(UINT, 0)))

end extern
