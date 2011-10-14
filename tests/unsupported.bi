/'
 * Functions returning procptrs:
 *        type '(' '*' id '(' params1 ')' ')' '(' params2 ')'
 *    ->
 *        DECLARE FUNCTION id '(' params1 ')' AS {SUB | FUNCTION} '(' params2 ')' [AS type]
 *
 *  1. int (*(*f(1))(2))(3);
 *  2. int f(1)(2)(3);
 *  3. f(1)(2)(3) int;
 *  4. declare function f(1) as function(2) as function(3) as integer
 '/
/' TODO: token 93 '/ (*(*f(/' TODO: token 93 '/))(/' TODO: token 93 '/))(/' TODO: token 93 '/)/' TODO: token 44 '/

/'
 * procptrs to functions returning procptrs:
 *
 *  1. int (*(*p)(1))(2);
 *  2. int p(1)(2);
 *  3. (1)(2) int p;
 *  4. as function(1) as function(2) as integer p
 '/
/' TODO: token 93 '/ (*(*p)(/' TODO: token 93 '/))(/' TODO: token 93 '/)/' TODO: token 44 '/

/'
 * Extra parentheses around the identifier in procptrs/procdecls
 *    id-expression = '(' ['*'] id-expression ')'
 *    id-expression = id
 *
 * Can be translated, but it needs a proper recursive parser...
 '/
/' TODO: token 93 '/ (f)(/' TODO: token 93 '/)/' TODO: token 44 '/
/' TODO: token 93 '/ (*(*(p))(/' TODO: token 93 '/))(/' TODO: token 93 '/)/' TODO: token 44 '/
/' TODO: token 93 '/ (*(*(((f)))(/' TODO: token 93 '/))(/' TODO: token 93 '/))(/' TODO: token 93 '/)/' TODO: token 44 '/

/'
 * Function pointer pointers -- only possible in FB using typedefs
 * So currently procedure pointer pointers aren't handled at all, even though
 * it would work for subs.
 *    int (**a)();     ->    dim as function() as integer ptr a (wrong)
 *    int *(*a)();     ->    dim as function() as integer ptr a
 *    void *(*a)();    ->    dim as function() as any ptr a
 *    void (*a)();     ->    dim as sub() a
 *    void (**a)();    ->    dim as sub() ptr a
 '/
/' TODO: token 93 '/ (**pp)()/' TODO: token 44 '/

/'
 * typedefs to procdecls (FB doesn't know something like that):
 *
 *    typedef int T(int);
 *    T t;
 *    t(5);
 *
 * Same as this, right?
 *
 *    int t(int);
 *    t(5);
 '/
/' TODO: token 105 '/ declare function T(byval as integer) as integer

enum
#if 1
	A    /' next token is '#' instead of ',' or '}', preventing the translation '/
#endif
end enum

/' Cannot have #directives mixed into the expression in FB '/
enum
	A =
	1
#if 1
	+
	2
#endif
	,
	B
end enum

/' Named nested unions/structs -- not supported in FB '/
/' TODO: token 103 '/ T {
	union {
		dim shared as integer a
	} foo/' TODO: token 44 '/
}/' TODO: token 44 '/
