#pragma once

type PackedStruct field = 1
	byte as byte
	dword as long
end type

type NormalStruct
	byte as byte
	dword as long
end type

const A = 1
dim shared a_1 as long = 1
dim shared b_1 as long = 1
#undef A
const A = 2
dim shared c_2 as long = 2
dim shared d_1 as long = 1
