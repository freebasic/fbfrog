#pragma once

extern "C"

type A
	'' TODO: unrecognized construct:
	'' x;
	'' ---------------------------------------------------------------------------
	'' tests/nested-unknowns.h(4): expected identifier for the symbol declared in this declaration but found ';'
	''      x;
	''       ^
	'' context as seen by fbfrog:
	''     struct A { x ; } ;
	''                  ^
end type

union B
	'' TODO: unrecognized construct:
	'' x;
	'' ---------------------------------------------------------------------------
	'' tests/nested-unknowns.h(8): expected identifier for the symbol declared in this declaration but found ';'
	''      x;
	''       ^
	'' context as seen by fbfrog:
	''     union B { x ; } ;
	''                 ^
end union

type C as long
enum
	'' TODO: unrecognized construct:
	'' x;
	'' ---------------------------------------------------------------------------
	'' tests/nested-unknowns.h(12): expected ',' or '}' behind enum constant but found ';'
	''      x;
	''       ^
	'' context as seen by fbfrog:
	''     enum C { x ; } ;
	''                ^
end enum

sub f()
	'' TODO: unrecognized construct:
	'' x=;
	'' ---------------------------------------------------------------------------
	'' tests/nested-unknowns.h(16): expected ';' (end of expression statement) but found '='
	''      x=;
	''       ^
	'' context as seen by fbfrog:
	''     void f ( void ) { x = ; }
	''                         ^
end sub

end extern
