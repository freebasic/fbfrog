#pragma once

extern "C"

dim shared id_id as long
declare sub idid()
dim shared id_param as long
declare sub idarg()
declare sub idaa()
declare sub idb()
declare sub id()
dim shared param_id as long
declare sub argid()
declare sub aaid()
declare sub bid()
declare sub id()
dim shared param1_param2 as long
declare sub arg1arg2()
declare sub ab()
declare sub b()
declare sub a()
dim shared id1_param_id2 as long
declare sub id1argid2()
dim shared param_id1_id2 as long
declare sub argid1id2()
dim shared id1_id2_param as long
declare sub id1id2arg()
dim shared param1_id_param2 as long
declare sub arg1idarg2()
dim shared param1_param2_param3 as long
declare sub abc()

dim shared merge_affects_only_first_or_last_token_of_an_arg as long
dim shared A as left_UDT
dim shared A_right as UDT
dim shared A_right as left_UDT
dim shared spacedid_param as long
declare sub spacedidarg()
dim shared id_spacedparam as long
declare sub idarg()
dim shared spacedid_spacedparam as long
declare sub spacedidarg()
dim shared spacedid_spacedparam as long
declare sub spacedidarg()
dim shared id_mm_decnum_to_id as long

type E as long
enum
	A = id1
	A = id1
end enum

dim shared id_mm_hexnum_to_id as long

type E as long
enum
	A = id0x1
	A = id0x1
end enum

dim shared id_mm_octnum_to_id as long

type E as long
enum
	A = id01
	A = id01
end enum

dim shared id_mm_kw_to_id as long

type E as long
enum
	A = idstruct
	A = idstruct
end enum

dim shared kw_mm_id_to_id as long

type E as long
enum
	A = structid
	A = structid
end enum

dim shared decnum_mm_id_to_hexnum as long

type E as long
enum
	A = &hFF
	A = &hFF
end enum

dim shared decnum_mm_decnum_to_decnum as long

type E as long
enum
	A = 11
	A = 11
end enum

dim shared decnum_mm_decnum_to_octnum as long

type E as long
enum
	A = &o100
	A = &o100
end enum

dim shared decnum_mm_octnum_to_decnum as long

type E as long
enum
	A = 101
	A = 101
end enum

dim shared octnum_mm_decnum_to_octnum as long

type E as long
enum
	A = &o11
	A = &o11
end enum

dim shared hexnum_mm_id_to_hexnum as long

type E as long
enum
	A = &hAABB
	A = &hAABB
end enum

dim shared hexnum_mm_decnum_to_hexnum as long

type E as long
enum
	A = &hAA123
	A = &hAA123
end enum

dim shared hexnum_mm_octnum_to_hexnum as long

type E as long
enum
	A = &hAA0123
	A = &hAA0123
end enum

dim shared decnum_mm_id_mm_decnum_to_hexnum as long

type E as long
enum
	A = &h100
	A = &h100
end enum

dim shared decnum_mm_id_mm_id_to_hexnum as long

type E as long
enum
	A = &hFF
	A = &hFF
end enum

dim shared decnum_mm_id_mm_id_to_hexnum as long

type E as long
enum
	A = &hFF
	A = &hFF
end enum

dim shared __1_0 as long

type E as long
enum
	A = 10
	A = 10
end enum

dim shared __0_1 as long

type E as long
enum
	A = &o1
	A = &o1
end enum

dim shared __0_0 as long

type E as long
enum
	A = &o0
	A = &o0
end enum

dim shared __123_000 as long

type E as long
enum
	A = 123000
	A = 123000
end enum

dim shared __000_123 as long

type E as long
enum
	A = &o00123
	A = &o00123
end enum

dim shared __param_0 as long

type E as long
enum
	A = &o0000
	A = &o0000
	B = 10
	B = 10
	C = 1230
	C = 1230
	D = arg0
	D = arg0
	E = &h0
	E = &h0
end enum

dim shared __param_00 as long

type E as long
enum
	A = &o00000
	A = &o00000
	B = 100
	B = 100
	C = 12300
	C = 12300
	D = arg00
	D = arg00
	E = &h00
	E = &h00
end enum

dim shared __param_123 as long

type E as long
enum
	A = arg123
	A = arg123
	B = 123123
	B = 123123
	C = &h123
	C = &h123
	D = &hF123
	D = &hF123
	E = &o123
	E = &o123
	F = &o123123
	F = &o123123
end enum

dim shared __param_FF as long

type E as long
enum
	A = argFF
	A = argFF
	B = &hFF
	B = &hFF
	C = &hAAFF
	C = &hAAFF
	D = FF
	D = FF
end enum

dim shared __param_aa as long

type E as long
enum
	A = argaa
	A = argaa
	B = &haa
	B = &haa
	C = &hAAaa
	C = &hAAaa
	D = aa
	D = aa
end enum

dim shared __param_0x as long

type E as long
enum
	A = arg0x
	A = arg0x
	B = &h
	B = &h
end enum

dim shared __0_param as long

type E as long
enum
	A = &o0000
	A = &o0000
	B = &o1
	B = &o1
	C = &o123
	C = &o123
	D = &o255
	D = &o255
	E = &hFF
	E = &hFF
	F = &h0
	F = &h0
	G = 0
	G = 0
end enum

dim shared __L_char as long
declare sub f(byval w as wchar_t = asc(wstr("a")))
dim shared __L_string as long
declare sub f(byval w as wstring ptr = wstr("abc"))

end extern
