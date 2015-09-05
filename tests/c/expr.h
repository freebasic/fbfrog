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
	A = (void (*)(void))0,
	A = (__attribute__((cdecl)) void (*)(void))0,
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
	A = sizeof (void (*)(void)),
	A = sizeof (__attribute__((cdecl)) void (*)(void)),
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

	ENUMCONST1 = 0,
};

#define A01(x) "a" "b"
#define A02(x) "a" "b" "c" "d"
#define A03(x) "a""b"
#define A04(x) "a" #x "b"
#define A05(x) "a"#x"b"#x"c"
#define A01_1 a b c
#define A01_2 a "b" c
#define A01_3 "a" b "c"

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
#define A19 (void (*)(void))0
#define A20 (__attribute__((cdecl)) void (*)(void))0
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

#define A55 { int a[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }; }
#define A56 { int a, b; }

#define A57

// Testing cast operator precedence
#define MAKE_RGB(r,g,b) ((int)(((signed char)(r)|((unsigned short)((signed char)(g))<<8))|(((unsigned int)(signed char)(b))<<16)))

// A culng() cast needs to be added around this BOP, to ensure the result is
// truncated to 32bit in FB 64bit
#define A58 (0u - 100u)
#define A59 (0u - 100)
#define A60 (0 - 100)
#define A61 (0 - 100u)

// Similar situation here - but the cast is already there, and we shouldn't add
// another one.
#define A62 ((unsigned int)(0u - 100u))

// Expression type depending on the type of an existing symbol
#define A70 (A58 - 1u)
#define A71 (A58 - 1)
#define A72 ((unsigned int) A58 - 1u)
#define A73 ((unsigned int) A58 - 1)
#define A74 (ENUMCONST1 - 1u)
#define A75 (ENUMCONST1 - 1)
#define A76 ((unsigned int)ENUMCONST1 - 1u)
#define A77 ((unsigned int)ENUMCONST1 - 1)
#define A78 (undefined - 1u)
#define A79 (undefined - 1)
#define A80 ((unsigned int)undefined - 1u)
#define A81 ((unsigned int)undefined - 1)
#define A82 ((int)undefined - 1u)
#define A83 ((int)undefined - 1)
#ifdef _WIN32
	#define A84 undefined
#else
	#define A84 (-1)
#endif
#define A85 A84

// Same but with forward references
#define B01 B00 // simple alias
#define B00 (0u - 100u)

#define B11 (B10 - 1) // "complex" math expression
#define B10 (0u - 100u)

#define B22 (B21 - 1) // extra indirection level
#define B21 (B20 - 1)
#define B20 (0u - 100u)

#define B31 (B30 - 1)
#define B32 (B31 - 1) // isn't a fwdref itself, but refers to one
#define B30 (0u - 100u)

#define A90(x) &(x)->a[(x)->b]

#define A100 struct { int i; }
#define A101 struct UDT { int i; }
#define A102 sizeof(struct { int i; })
#define A103 sizeof(struct UDT { int i; })
#define A104 (struct { int i; })x
#define A105 (struct UDT { int i; })x

// "a b" is probably a vardecl here, and it shouldn't be misparsed as string
// literal sequence silently.
#define A110 { a b; }

// Testing the handling of parenthesized macro parameters
// In general, fbfrog should
//  - preserve parentheses around macro parameters
//  - not add unnecessary parentheses
//  - maybe add parentheses where they were missed/unnecessary in C, but would
//    be useful in FB
#define mysizeof1(x) sizeof(x) // fbfrog shouldn't emit extra parentheses around the macro parameter here
#define mysizeof2(x) sizeof((x)) // but here it probably should
#define A120a(x) f(x, 1)
#define A120b(x) f((x), 1)
#define A121a(x) { 1, x, 3 }
#define A121b(x) { 1, (x), 3 }
#define A122a(x) x = 1
#define A122b(x) (x) = 1
#define A123a(x) ((int)x)
#define A123b(x) ((int)(x))
#define A124a(x) ((void *)x)
#define A124b(x) ((void *)(x))
#define A125a(x) (x ? x : x)
#define A125b(x) ((x) ? (x) : (x))

// Complex function calls
#define NormalCall1 myFunction()
#define NormalCall2 myFunction(1, 2)
#define NormalCall3 myFunctionPtr(1, 2)
#define NormalCall4(fn) fn(1, 2)
#define ComplexCall1 (myFunction)()
#define ComplexCall2 (myFunction)(1, 2)
#define ComplexCall3 ((myFunction))(1, 2)
#define ComplexCall4 (*myFunctionPtr)(1, 2)
#define ComplexCall5 (*(myFunctionPtr))(1, 2)
#define ComplexCall6(fn) (fn)(1, 2)
