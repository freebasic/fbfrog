// @fbfrog -removedefine m1

static int id_id;
#define m1 void id##id(void);
m1
#undef m1

static int id_param;
#define m1(param) void id##param(void);
m1(arg)
m1(aa)
m1(b)
m1()
#undef m1

static int param_id;
#define m1(param) void param##id(void);
m1(arg)
m1(aa)
m1(b)
m1()
#undef m1

static int param1_param2;
#define m1(param1, param2) void param1##param2(void);
m1(arg1, arg2)
m1(a, b)
m1(, b)
m1(a, )
#undef m1

static int id1_param_id2;
#define m1(param) void id1##param##id2(void);
m1(arg)
#undef m1

static int param_id1_id2;
#define m1(param) void param##id1##id2(void);
m1(arg)
#undef m1

static int id1_id2_param;
#define m1(param) void id1##id2##param(void);
m1(arg)
#undef m1

static int param1_id_param2;
#define m1(param1, param2) void param1##id##param2(void);
m1(arg1, arg2)
#undef m1

static int param1_param2_param3;
#define m1(param1, param2, param3) void param1##param2##param3(void);
m1(a,b,c)
#undef m1

static int merge_affects_only_first_or_last_token_of_an_arg;
#define m1(param) static struct left_##param;
m1(UDT A)
#undef m1
#define m1(param) static struct param##_right;
m1(UDT A)
#undef m1
#define m1(param) static struct left_##param##_right;
m1(UDT A)
#undef m1

static int spacedid_param;
#define m1(param) void spacedid ##param(void);
m1(arg)
#undef m1

static int id_spacedparam;
#define m1(spacedparam) void id## spacedparam(void);
m1(arg)
#undef m1

static int spacedid_spacedparam;
#define m1(spacedparam) void spacedid ## spacedparam(void);
m1(arg)
#undef m1

static int spacedid_spacedparam;
#define m1(spacedparam) void spacedid        ##      spacedparam(void);
m1(arg)
#undef m1

//------------------------------------------------------------------------------

static int id_mm_decnum_to_id;
#define m1 id##1
enum E {
	A = id1,
	A = m1
};
#undef m1

static int id_mm_hexnum_to_id;
#define m1 id##0x1
enum E {
	A = id0x1,
	A = m1
};
#undef m1

static int id_mm_octnum_to_id;
#define m1 id##01
enum E {
	A = id01,
	A = m1
};
#undef m1

static int id_mm_kw_to_id;
#define m1 id##struct
enum E {
	A = idstruct,
	A = m1
};
#undef m1

static int kw_mm_id_to_id;
#define m1 struct##id
enum E {
	A = structid,
	A = m1
};
#undef m1

//------------------------------------------------------------------------------

static int decnum_mm_id_to_hexnum;
#define m1 0##xFF
enum E {
	A = 0xFF,
	A = m1
};
#undef m1

static int decnum_mm_decnum_to_decnum;
#define m1 1##1
enum E {
	A = 11,
	A = m1
};
#undef m1

static int decnum_mm_decnum_to_octnum;
#define m1 0##100
enum E {
	A = 0100,
	A = m1
};
#undef m1

static int decnum_mm_octnum_to_decnum;
#define m1 1##01
enum E {
	A = 101,
	A = m1
};
#undef m1

//------------------------------------------------------------------------------

static int octnum_mm_decnum_to_octnum;
#define m1 01##1
enum E {
	A = 011,
	A = m1
};
#undef m1

//------------------------------------------------------------------------------

static int hexnum_mm_id_to_hexnum;
#define m1 0xAA##BB
enum E {
	A = 0xAABB,
	A = m1
};
#undef m1

static int hexnum_mm_decnum_to_hexnum;
#define m1 0xAA##123
enum E {
	A = 0xAA123,
	A = m1
};
#undef m1

static int hexnum_mm_octnum_to_hexnum;
#define m1 0xAA##0123
enum E {
	A = 0xAA0123,
	A = m1
};
#undef m1

//------------------------------------------------------------------------------

static int decnum_mm_id_mm_decnum_to_hexnum;
#define m1 0##x##100
enum E {
	A = 0x100,
	A = m1
};
#undef m1

static int decnum_mm_id_mm_id_to_hexnum;
#define m1 0##x##FF
enum E {
	A = 0xFF,
	A = m1
};
#undef m1

static int decnum_mm_id_mm_id_to_hexnum;
#define m1 0##x##FF
enum E {
	A = 0xFF,
	A = m1
};
#undef m1

//------------------------------------------------------------------------------

static int __1_0;
#define m1 1 ## 0
enum E {
	A = 10,
	A = m1
};
#undef m1

static int __0_1;
#define m1 0 ## 1
enum E {
	A = 01,
	A = m1
};
#undef m1

static int __0_0;
#define m1 0 ## 0
enum E {
	A = 00,
	A = m1
};
#undef m1

static int __123_000;
#define m1 123 ## 000
enum E {
	A = 123000,
	A = m1
};
#undef m1

static int __000_123;
#define m1 000 ## 123
enum E {
	A = 000123,
	A = m1
};
#undef m1

//------------------------------------------------------------------------------

static int __param_0;
#define m1(param) param##0
enum E {
	A = 00000,
	A = m1(0000),
	B = 10,
	B = m1(1),
	C = 1230,
	C = m1(123),
	D = arg0,
	D = m1(arg),
	E = 0x0,
	E = m1(0x)
};
#undef m1

static int __param_00;
#define m1(param) param##00
enum E {
	A = 000000,
	A = m1(0000),
	B = 100,
	B = m1(1),
	C = 12300,
	C = m1(123),
	D = arg00,
	D = m1(arg),
	E = 0x00,
	E = m1(0x)
};
#undef m1

static int __param_123;
#define m1(param) param##123
enum E {
	A = arg123,
	A = m1(arg),
	B = 123123,
	B = m1(123),
	C = 0x123,
	C = m1(0x),
	D = 0xF123,
	D = m1(0xF),
	E = 0123,
	E = m1(0),
	F = 0123123,
	F = m1(0123)
};
#undef m1

static int __param_FF;
#define m1(param) param##FF
enum E {
	A = argFF,
	A = m1(arg),
	B = 0xFF,
	B = m1(0x),
	C = 0xAAFF,
	C = m1(0xAA),
	D = FF,
	D = m1()
};
#undef m1

static int __param_aa;
#define m1(param) param##aa
enum E {
	A = argaa,
	A = m1(arg),
	B = 0xaa,
	B = m1(0x),
	C = 0xAAaa,
	C = m1(0xAA),
	D = aa,
	D = m1()
};
#undef m1

static int __param_0x;
#define m1(param) param##0x
enum E {
	A = arg0x,
	A = m1(arg),
	B = 0x,
	B = m1()
};
#undef m1

//------------------------------------------------------------------------------

static int __0_param;
#define m1(param) 0##param
enum E {
	A = 00000,
	A = m1(0000),
	B = 01,
	B = m1(1),
	C = 0123,
	C = m1(123),
	D = 0255,
	D = m1(255),
	E = 0xFF,
	E = m1(xFF),
	F = 0x0,
	F = m1(x0),
	G = 0,
	G = m1()
};
#undef m1

//------------------------------------------------------------------------------

static int __L_char;
#define m1 L ## 'a'
void f(wchar_t w = m1);
#undef m1

static int __L_string;
#define m1 L ## "abc"
void f(wchar_t *w = m1);
#undef m1
