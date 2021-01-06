#pragma once

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
