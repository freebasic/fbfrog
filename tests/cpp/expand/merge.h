static int id_id;
#define EXPANDME1 void id##id(void);
EXPANDME1
#undef EXPANDME1

static int id_param;
#define EXPANDME1(param) void id##param(void);
EXPANDME1(arg)
EXPANDME1(aa)
EXPANDME1(b)
EXPANDME1()
#undef EXPANDME1

static int param_id;
#define EXPANDME1(param) void param##id(void);
EXPANDME1(arg)
EXPANDME1(aa)
EXPANDME1(b)
EXPANDME1()
#undef EXPANDME1

static int param1_param2;
#define EXPANDME1(param1, param2) void param1##param2(void);
EXPANDME1(arg1, arg2)
EXPANDME1(a, b)
EXPANDME1(, b)
EXPANDME1(a, )
#undef EXPANDME1

static int id1_param_id2;
#define EXPANDME1(param) void id1##param##id2(void);
EXPANDME1(arg)
#undef EXPANDME1

static int param_id1_id2;
#define EXPANDME1(param) void param##id1##id2(void);
EXPANDME1(arg)
#undef EXPANDME1

static int id1_id2_param;
#define EXPANDME1(param) void id1##id2##param(void);
EXPANDME1(arg)
#undef EXPANDME1

static int param1_id_param2;
#define EXPANDME1(param1, param2) void param1##id##param2(void);
EXPANDME1(arg1, arg2)
#undef EXPANDME1

static int param1_param2_param3;
#define EXPANDME1(param1, param2, param3) void param1##param2##param3(void);
EXPANDME1(a,b,c)
#undef EXPANDME1

static int merge_affects_only_first_or_last_token_of_an_arg;
#define EXPANDME1(param) typedef struct id##param;
EXPANDME1(UDT A)
#undef EXPANDME1
#define EXPANDME1(param) typedef struct param##id;
EXPANDME1(UDT A)
#undef EXPANDME1
#define EXPANDME1(param) typedef struct id1##param##id2;
EXPANDME1(UDT A)
#undef EXPANDME1

static int spacedid_param;
#define EXPANDME1(param) void spacedid ##param(void);
EXPANDME1(arg)
#undef EXPANDME1

static int id_spacedparam;
#define EXPANDME1(spacedparam) void id## spacedparam(void);
EXPANDME1(arg)
#undef EXPANDME1

static int spacedid_spacedparam;
#define EXPANDME1(spacedparam) void spacedid ## spacedparam(void);
EXPANDME1(arg)
#undef EXPANDME1

static int spacedid_spacedparam;
#define EXPANDME1(spacedparam) void spacedid        ##      spacedparam(void);
EXPANDME1(arg)
#undef EXPANDME1

//------------------------------------------------------------------------------

static int id_mm_decnum_to_id;
#define EXPANDME1 id##1
enum E {
	A = id1,
	A = EXPANDME1
};
#undef EXPANDME1

static int id_mm_hexnum_to_id;
#define EXPANDME1 id##0x1
enum E {
	A = id0x1,
	A = EXPANDME1
};
#undef EXPANDME1

static int id_mm_octnum_to_id;
#define EXPANDME1 id##01
enum E {
	A = id01,
	A = EXPANDME1
};
#undef EXPANDME1

static int id_mm_kw_to_id;
#define EXPANDME1 id##struct
enum E {
	A = idstruct,
	A = EXPANDME1
};
#undef EXPANDME1

static int kw_mm_id_to_id;
#define EXPANDME1 struct##id
enum E {
	A = structid,
	A = EXPANDME1
};
#undef EXPANDME1

//------------------------------------------------------------------------------

static int decnum_mm_id_to_hexnum;
#define EXPANDME1 0##xFF
enum E {
	A = 0xFF,
	A = EXPANDME1
};
#undef EXPANDME1

static int decnum_mm_decnum_to_decnum;
#define EXPANDME1 1##1
enum E {
	A = 11,
	A = EXPANDME1
};
#undef EXPANDME1

static int decnum_mm_decnum_to_octnum;
#define EXPANDME1 0##100
enum E {
	A = 0100,
	A = EXPANDME1
};
#undef EXPANDME1

static int decnum_mm_octnum_to_decnum;
#define EXPANDME1 1##01
enum E {
	A = 101,
	A = EXPANDME1
};
#undef EXPANDME1

//------------------------------------------------------------------------------

static int octnum_mm_decnum_to_octnum;
#define EXPANDME1 01##1
enum E {
	A = 011,
	A = EXPANDME1
};
#undef EXPANDME1

//------------------------------------------------------------------------------

static int hexnum_mm_id_to_hexnum;
#define EXPANDME1 0xAA##BB
enum E {
	A = 0xAABB,
	A = EXPANDME1
};
#undef EXPANDME1

static int hexnum_mm_decnum_to_hexnum;
#define EXPANDME1 0xAA##123
enum E {
	A = 0xAA123,
	A = EXPANDME1
};
#undef EXPANDME1

static int hexnum_mm_octnum_to_hexnum;
#define EXPANDME1 0xAA##0123
enum E {
	A = 0xAA0123,
	A = EXPANDME1
};
#undef EXPANDME1

//------------------------------------------------------------------------------

static int decnum_mm_id_mm_decnum_to_hexnum;
#define EXPANDME1 0##x##100
enum E {
	A = 0x100,
	A = EXPANDME1
};
#undef EXPANDME1

static int decnum_mm_id_mm_id_to_hexnum;
#define EXPANDME1 0##x##FF
enum E {
	A = 0xFF,
	A = EXPANDME1
};
#undef EXPANDME1

static int decnum_mm_id_mm_id_to_hexnum;
#define EXPANDME1 0##x##FF
enum E {
	A = 0xFF,
	A = EXPANDME1
};
#undef EXPANDME1

//------------------------------------------------------------------------------

static int __1_0;
#define EXPANDME1 1 ## 0
enum E {
	A = 10,
	A = EXPANDME1
};
#undef EXPANDME1

static int __0_1;
#define EXPANDME1 0 ## 1
enum E {
	A = 01,
	A = EXPANDME1
};
#undef EXPANDME1

static int __0_0;
#define EXPANDME1 0 ## 0
enum E {
	A = 00,
	A = EXPANDME1
};
#undef EXPANDME1

static int __123_000;
#define EXPANDME1 123 ## 000
enum E {
	A = 123000,
	A = EXPANDME1
};
#undef EXPANDME1

static int __000_123;
#define EXPANDME1 000 ## 123
enum E {
	A = 000123,
	A = EXPANDME1
};
#undef EXPANDME1

//------------------------------------------------------------------------------

static int __param_0;
#define EXPANDME1(param) param##0
enum E {
	A = 00000,
	A = EXPANDME1(0000),
	B = 10,
	B = EXPANDME1(1),
	C = 1230,
	C = EXPANDME1(123),
	D = arg0,
	D = EXPANDME1(arg),
	E = 0x0,
	E = EXPANDME1(0x)
};
#undef EXPANDME1

static int __param_00;
#define EXPANDME1(param) param##00
enum E {
	A = 000000,
	A = EXPANDME1(0000),
	B = 100,
	B = EXPANDME1(1),
	C = 12300,
	C = EXPANDME1(123),
	D = arg00,
	D = EXPANDME1(arg),
	E = 0x00,
	E = EXPANDME1(0x)
};
#undef EXPANDME1

static int __param_123;
#define EXPANDME1(param) param##123
enum E {
	A = arg123,
	A = EXPANDME1(arg),
	B = 123123,
	B = EXPANDME1(123),
	C = 0x123,
	C = EXPANDME1(0x),
	D = 0xF123,
	D = EXPANDME1(0xF),
	E = 0123,
	E = EXPANDME1(0),
	F = 0123123,
	F = EXPANDME1(0123)
};
#undef EXPANDME1

static int __param_FF;
#define EXPANDME1(param) param##FF
enum E {
	A = argFF,
	A = EXPANDME1(arg),
	B = 0xFF,
	B = EXPANDME1(0x),
	C = 0xAAFF,
	C = EXPANDME1(0xAA),
	D = FF,
	D = EXPANDME1()
};
#undef EXPANDME1

static int __param_aa;
#define EXPANDME1(param) param##aa
enum E {
	A = argaa,
	A = EXPANDME1(arg),
	B = 0xaa,
	B = EXPANDME1(0x),
	C = 0xAAaa,
	C = EXPANDME1(0xAA),
	D = aa,
	D = EXPANDME1()
};
#undef EXPANDME1

static int __param_0x;
#define EXPANDME1(param) param##0x
enum E {
	A = arg0x,
	A = EXPANDME1(arg),
	B = 0x,
	B = EXPANDME1()
};
#undef EXPANDME1

//------------------------------------------------------------------------------

static int __0_param;
#define EXPANDME1(param) 0##param
enum E {
	A = 00000,
	A = EXPANDME1(0000),
	B = 01,
	B = EXPANDME1(1),
	C = 0123,
	C = EXPANDME1(123),
	D = 0255,
	D = EXPANDME1(255),
	E = 0xFF,
	E = EXPANDME1(xFF),
	F = 0x0,
	F = EXPANDME1(x0),
	G = 0,
	G = EXPANDME1()
};
#undef EXPANDME1

//------------------------------------------------------------------------------

static int __L_char;
#define EXPANDME1 L ## 'a'
void f(wchar_t w = EXPANDME1);
#undef EXPANDME1

static int __L_string;
#define EXPANDME1 L ## "abc"
void f(wchar_t *w = EXPANDME1);
#undef EXPANDME1
