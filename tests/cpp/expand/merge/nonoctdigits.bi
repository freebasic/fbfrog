#pragma once

type E as long
enum
	'' TODO: unrecognized construct:
	'' A = 08
	'' ---------------------------------------------------------------------------
	'' invalid digit in octal number literal
	''     enum E { A = 08 } ;
	''                  ^~
	'' tests/cpp/expand/merge/nonoctdigits.h(3): construct found here
	''     enum E {
	''     ^~~~
	'' ## merge operation(1): token found here
	''      08
	''      ^~
	'' tests/cpp/expand/merge/nonoctdigits.h(2): from here:
	''     #define m 0 ## 8
	''                 ^~
end enum
