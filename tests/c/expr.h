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

#define A(x) "a" "b"
#define A(x) "a" "b" "c" "d"
#define A(x) "a""b"
#define A(x) "a" #x "b"
#define A(x) "a"#x"b"#x"c"

#define A (void)0
#define A (char)0
#define A (float)0
#define A (double)0
#define A (int)0
#define A (signed int)0
#define A (unsigned int)0
#define A (const int)0
#define A (short)0
#define A (long)0
#define A (enum E)0
#define A (struct UDT)0
#define A (union UDT)0
#define A (__stdcall void (*)(void))0
#define A (__cdecl void (*)(void))0
#define A (__attribute__((stdcall)) void (*)(void))0
#define A (void *)0
#define A (int *)0
#define A (struct UDT *)0
#define A (union UDT *)0
#define A (wchar_t)0
#define A (size_t)0
#define A (ssize_t)0
#define A (ptrdiff_t)0
#define A (int8_t)0
#define A (uint8_t)0
#define A (int16_t)0
#define A (uint16_t)0
#define A (int32_t)0
#define A (uint32_t)0
#define A (int64_t)0
#define A (uint64_t)0
#define A (__int8)0
#define A (__int16)0
#define A (__int32)0
#define A (__int64)0
#define A (MYINT)0

#define A { a(0); b(1); }
#define A { 1, 2, 3 }
#define A { }
#define A { 1 }
#define A { 1, }

// C UOP nested inside children list
#define A { !a, !a }
#define A { f( !a ); }
