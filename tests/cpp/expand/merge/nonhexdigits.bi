#pragma once

type E as long
enum
	'' TODO: unrecognized construct:
	'' A =0xG
	'' ---------------------------------------------------------------------------
	'' invalid suffix on number literal: 'G'
	''     enum E { A = 0xG } ;
	''                  ^~~
	'' tests/cpp/expand/merge/nonhexdigits.h(3): construct found here
	''     enum E {
	''     ^~~~
	'' ## merge operation(1): token found here
	''     0xG
	''     ^~~
	'' tests/cpp/expand/merge/nonhexdigits.h(2): from here:
	''     #define m 0x ## G
	''                  ^~
end enum
