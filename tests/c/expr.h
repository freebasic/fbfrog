// @fbfrog -whitespace -nonamefixup

typedef int MYINT;

enum E {
	// atoms
	A = (A),
	A = ((A)),
	A = (void)0,
	A = (char)0,
	A = (float)0,
	A = (double)0,
	A = (int)0,
	A = (signed int)0,
	A = (unsigned int)0,
	A = (const int)0,
	A = (short)0,
	A = (long)0,
	A = (enum E)0,
	A = (struct UDT)0,
	A = (union UDT)0,
	A = (__stdcall void (*)(void))0,
	A = (__cdecl void (*)(void))0,
	A = (__attribute__((stdcall)) void (*)(void))0,
	A = (void *)0,
	A = (int *)0,
	A = (struct UDT *)0,
	A = (union UDT *)0,
	A = (wchar_t)0,
	A = (size_t)0,
	A = (ssize_t)0,
	A = (ptrdiff_t)0,
	A = (int8_t)0,
	A = (uint8_t)0,
	A = (int16_t)0,
	A = (uint16_t)0,
	A = (int32_t)0,
	A = (uint32_t)0,
	A = (int64_t)0,
	A = (uint64_t)0,
	A = (__int8)0,
	A = (__int16)0,
	A = (__int32)0,
	A = (__int64)0,
	A = (MYINT)0,
	A = 1,
	A = 01,
	A = 0x1,
	A = "a",
	A = L"a",
	A = 'a',
	A = L'a',
	A = A,
	A = f(A),
	A = f(1,2,3,4),
	A = f(),

	// UOPs
	A = !A,
	A = ~A,
	A = -A,
	A = +A,
	A = &A,
	A = *A,
	A = sizeof A,
	A = sizeof(A),
	A = sizeof (int),
	A = sizeof (int) * 2,
	A = sizeof (int) << 1,
	A = sizeof (MYINT),
	A = sizeof (MYINT) * 2,
	A = sizeof (MYINT) << 1,
	A = sizeof (__attribute__((stdcall)) void (*)(void)),

	A = !a,
	A = ~a,
	A = -a,
	A = +a,

	A = (!a),
	A = (~a),
	A = (-a),
	A = (+a),

	A = !(a),
	A = ~(a),
	A = -(a),
	A = +(a),

	A = ! ! a,
	A = ~ ~ a,
	A = - - a,
	A = + + a,

	A = a || - - b,
	A = a || - - b,
	A = a && - - b,
	A = a |  - - b,
	A = a ^  - - b,
	A = a &  - - b,
	A = a == - - b,
	A = a != - - b,
	A = a <  - - b,
	A = a <= - - b,
	A = a >  - - b,
	A = a >= - - b,
	A = a << - - b,
	A = a >> - - b,
	A = a +  - - b,
	A = a -  - - b,
	A = a *  - - b,
	A = a /  - - b,
	A = a %  - - b,

	A =  - -  a  || b,
	A =  - - (a  || b),
	A = (- -  a) || b,

	A =  - -  a  && b,
	A =  - - (a  && b),
	A = (- -  a) && b,

	A =  - -  a  |  b,
	A =  - - (a  |  b),
	A = (- -  a) |  b,

	A =  - -  a  ^  b,
	A =  - - (a  ^  b),
	A = (- -  a) ^  b,

	A =  - -  a  &  b,
	A =  - - (a  &  b),
	A = (- -  a) &  b,

	A =  - -  a  == b,
	A =  - - (a  == b),
	A = (- -  a) == b,

	A =  - -  a  != b,
	A =  - - (a  != b),
	A = (- -  a) != b,

	A =  - -  a  <  b,
	A =  - - (a  <  b),
	A = (- -  a) <  b,

	A =  - -  a  <= b,
	A =  - - (a  <= b),
	A = (- -  a) <= b,

	A =  - -  a  >  b,
	A =  - - (a  >  b),
	A = (- -  a) >  b,

	A =  - -  a  >= b,
	A =  - - (a  >= b),
	A = (- -  a) >= b,

	A =  - -  a  << b,
	A =  - - (a  << b),
	A = (- -  a) << b,

	A =  - -  a  >> b,
	A =  - - (a  >> b),
	A = (- -  a) >> b,

	A =  - -  a  +  b,
	A =  - - (a  +  b),
	A = (- -  a) +  b,

	A =  - -  a  -  b,
	A =  - - (a  -  b),
	A = (- -  a) -  b,

	A =  - -  a  *  b,
	A =  - - (a  *  b),
	A = (- -  a) *  b,

	A =  - -  a  /  b,
	A =  - - (a  /  b),
	A = (- -  a) /  b,

	A =  - -  a  %  b,
	A =  - - (a  %  b),
	A = (- -  a) %  b,

	// infix BOPs
	A = a ? b : c,
	A = a || b,
	A = a && b,
	A = a | b,
	A = a ^ b,
	A = a & b,
	A = a == b,
	A = a != b,
	A = a < b,
	A = a <= b,
	A = a > b,
	A = a >= b,
	A = a << b,
	A = a >> b,
	A = a + b,
	A = a - b,
	A = a * b,
	A = a / b,
	A = a % b,
	A = a[b],
	A = a.b,
	A = a->b,

	A = (a || b),
	A = (a && b),
	A = (a |  b),
	A = (a ^  b),
	A = (a &  b),
	A = (a == b),
	A = (a != b),
	A = (a <  b),
	A = (a <= b),
	A = (a >  b),
	A = (a >= b),
	A = (a << b),
	A = (a >> b),
	A = (a +  b),
	A = (a -  b),
	A = (a *  b),
	A = (a /  b),
	A = (a %  b),

	A = (a) || (b),
	A = (a) && (b),
	A = (a) |  (b),
	A = (a) ^  (b),
	A = (a) &  (b),
	A = (a) == (b),
	A = (a) != (b),
	A = (a) <  (b),
	A = (a) <= (b),
	A = (a) >  (b),
	A = (a) >= (b),
	A = (a) << (b),
	A = (a) >> (b),
	A = (a) +  (b),
	A = (a) -  (b),
	A = (a) *  (b),
	A = (a) /  (b),
	A = (a) %  (b),

	A =  a ||  b  || c,
	A =  a || (b  || c),
	A = (a ||  b) || c,
	A =  a ||  b  && c,
	A =  a || (b  && c),
	A = (a ||  b) && c,
	A =  a &&  b  || c,
	A =  a && (b  || c),
	A = (a &&  b) || c,

	A =  a &&  b  && c,
	A =  a && (b  && c),
	A = (a &&  b) && c,
	A =  a &&  b  |  c,
	A =  a && (b  |  c),
	A = (a &&  b) |  c,
	A =  a |   b  && c,
	A =  a |  (b  && c),
	A = (a |   b) && c,

	A =  a |   b  |  c,
	A =  a |  (b  |  c),
	A = (a |   b) |  c,
	A =  a |   b  ^  c,
	A =  a |  (b  ^  c),
	A = (a |   b) ^  c,
	A =  a ^   b  |  c,
	A =  a ^  (b  |  c),
	A = (a ^   b) |  c,

	A =  a ^   b  ^  c,
	A =  a ^  (b  ^  c),
	A = (a ^   b) ^  c,
	A =  a ^   b  &  c,
	A =  a ^  (b  &  c),
	A = (a ^   b) &  c,
	A =  a &   b  ^  c,
	A =  a &  (b  ^  c),
	A = (a &   b) ^  c,

	A =  a &   b  &  c,
	A =  a &  (b  &  c),
	A = (a &   b) &  c,
	A =  a &   b  == c,
	A =  a &  (b  == c),
	A = (a &   b) == c,
	A =  a ==  b  &  c,
	A =  a == (b  &  c),
	A = (a ==  b) &  c,

	A =  a ==  b  == c,
	A =  a == (b  == c),
	A = (a ==  b) == c,
	A =  a ==  b  != c,
	A =  a == (b  != c),
	A = (a ==  b) != c,
	A =  a !=  b  == c,
	A =  a != (b  == c),
	A = (a !=  b) == c,

	A =  a !=  b  != c,
	A =  a != (b  != c),
	A = (a !=  b) != c,
	A =  a !=  b  <  c,
	A =  a != (b  <  c),
	A = (a !=  b) <  c,
	A =  a <   b  != c,
	A =  a <  (b  != c),
	A = (a <   b) != c,

	A =  a <   b  <  c,
	A =  a <  (b  <  c),
	A = (a <   b) <  c,
	A =  a <   b  <= c,
	A =  a <  (b  <= c),
	A = (a <   b) <= c,
	A =  a <=  b  <  c,
	A =  a <= (b  <  c),
	A = (a <=  b) <  c,

	A =  a <=  b  <= c,
	A =  a <= (b  <= c),
	A = (a <=  b) <= c,
	A =  a <=  b  >  c,
	A =  a <= (b  >  c),
	A = (a <=  b) >  c,
	A =  a >   b  <= c,
	A =  a >  (b  <= c),
	A = (a >   b) <= c,

	A =  a >   b  >  c,
	A =  a >  (b  >  c),
	A = (a >   b) >  c,
	A =  a >   b  >= c,
	A =  a >  (b  >= c),
	A = (a >   b) >= c,
	A =  a >=  b  >  c,
	A =  a >= (b  >  c),
	A = (a >=  b) >  c,

	A =  a >=  b  >= c,
	A =  a >= (b  >= c),
	A = (a >=  b) >= c,
	A =  a >=  b  << c,
	A =  a >= (b  << c),
	A = (a >=  b) << c,
	A =  a <<  b  >= c,
	A =  a << (b  >= c),
	A = (a <<  b)  >= 1,

	A =  a <<  b  << c,
	A =  a << (b  << c),
	A = (a <<  b) << c,
	A =  a <<  b  >> c,
	A =  a << (b  >> c),
	A = (a <<  b) >> c,
	A =  a >>  b  << c,
	A =  a >> (b  << c),
	A = (a >>  b) << c,

	A =  a >>  b  >> c,
	A =  a >> (b  >> c),
	A = (a >>  b) >> c,
	A =  a >>  b  +  c,
	A =  a >> (b  +  c),
	A = (a >>  b) +  c,
	A =  a +   b  >> c,
	A =  a +  (b  >> c),
	A = (a +   b) >> c,

	A =  a +   b  +  c,
	A =  a +  (b  +  c),
	A = (a +   b) +  c,
	A =  a +   b  -  c,
	A =  a +  (b  -  c),
	A = (a +   b) -  c,
	A =  a -   b  +  c,
	A =  a -  (b  +  c),
	A = (a -   b) +  c,

	A =  a -   b  -  c,
	A =  a -  (b  -  c),
	A = (a -   b)  -  1,
	A =  a -   b  *  c,
	A =  a -  (b  *  c),
	A = (a -   b) *  c,
	A =  a *   b  -  c,
	A =  a *  (b  -  c),
	A = (a *   b) -  c,

	A =  a *   b  *  c,
	A =  a *  (b  *  c),
	A = (a *   b) *  c,
	A =  a *   b  /  c,
	A =  a *  (b  /  c),
	A = (a *   b) /  c,
	A =  a /   b  *  c,
	A =  a /  (b  *  c),
	A = (a /   b) *  c,

	A =  a /   b  /  c,
	A =  a /  (b  /  c),
	A = (a /   b) /  c,
	A =  a /   b  %  c,
	A =  a /  (b  %  c),
	A = (a /   b) %  c,
	A =  a %   b  /  c,
	A =  a %  (b  /  c),
	A = (a %   b) /  c,

	A =  a %   b  %  c,
	A =  a %  (b  %  c),
	A = (a %   b) %  c,

	A = a && b + c && d == e,

	A =  a ? b :  c,
	A =  a ? b :  c  ? d : e,
	A =  a ? b : (c  ? d : e),
	A = (a ? b :  c) ? d : e,

	A = { 1, 2, 3 },
	A = { },
	A = { 1 },
	A = { 1, },

	// C UOP nested inside children list
	A = { !a },
	A = f( !a ),
};

#define A01(x) "a" "b"
#define A02(x) "a" "b" "c" "d"
#define A03(x) "a""b"
#define A04(x) "a" #x "b"
#define A05(x) "a"#x"b"#x"c"

#define A06 (void)0
#define A07 (char)0
#define A08 (float)0
#define A09 (double)0
#define A10 (int)0
#define A11 (signed int)0
#define A12 (unsigned int)0
#define A13 (const int)0
#define A14 (short)0
#define A15 (long)0
#define A16 (enum E)0
#define A17 (struct UDT)0
#define A18 (union UDT)0
#define A19 (__stdcall void (*)(void))0
#define A20 (__cdecl void (*)(void))0
#define A21 (__attribute__((stdcall)) void (*)(void))0
#define A22 (void *)0
#define A23 (int *)0
#define A24 (struct UDT *)0
#define A25 (union UDT *)0
#define A26 (wchar_t)0
#define A27 (size_t)0
#define A28 (ssize_t)0
#define A29 (ptrdiff_t)0
#define A30 (int8_t)0
#define A31 (uint8_t)0
#define A32 (int16_t)0
#define A33 (uint16_t)0
#define A34 (int32_t)0
#define A35 (uint32_t)0
#define A36 (int64_t)0
#define A37 (uint64_t)0
#define A38 (__int8)0
#define A39 (__int16)0
#define A40 (__int32)0
#define A41 (__int64)0
#define A42 (MYINT)0

#define A43 { a(0); b(1); }
#define A44 { 1, 2, 3 }
#define A45 { }
#define A46 { 1 }
#define A47 { 1, }

// C UOP nested inside children list
#define A48 { !a, !a }
#define A49 { f( !a ); }

#define A50 defined(FOO)
#define A51(a) defined(a)

#define A52(T) ((T*)malloc(sizeof(T)))
#define A53 ((MYINT*)malloc(sizeof(MYINT)))
#define A54 ((const unsigned int ***)malloc(123))
