#pragma once

'' TODO: unrecognized construct:
'' #define A1 ({ int i = foo(0); i; })
'' ---------------------------------------------------------------------------
'' tests/c/statement-expression.h(2): expected expression but found '{'
''     #define A1 ({ int i = foo(0); i; })
''                 ^
'' context as seen by fbfrog:
''     # define A1 ( { int i = foo ( 0 ) ; i ; } ) 
''                   ^

#macro A2
	scope
		'' TODO: unrecognized construct:
		'' int a[({ f(); })];
		'' ---------------------------------------------------------------------------
		'' tests/c/statement-expression.h(4): expected expression but found '{'
		''     #define A2 { int a[({ f(); })]; }
		''                         ^
		'' context as seen by fbfrog:
		''     # define A2 { int a [ ( { f ( ) ; } ) ] ; } 
		''                             ^
	end scope
#endmacro
