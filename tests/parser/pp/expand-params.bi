#define EXPANDME1( a ) a
declare sub f( )
declare function f( byval as long, byval as long ) as long

#define EXPANDME2( a ) voida
declare sub bar( )
declare sub baz( )

#define EXPANDME3( a ) a(void);
declare sub bar( )
declare sub baz( )

#define EXPANDME4( a ) voida(void);
declare sub bar( )
declare sub baz( )

#define EXPANDME5( a, b, c ) abc
declare sub f( )

#define EXPANDME6( a, b, c ) cba
declare sub f( )
