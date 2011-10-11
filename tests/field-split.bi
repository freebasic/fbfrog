type T
	as integer a : as integer ptr ptr b, c
	as integer ptr aa : as integer bb, cc : as integer ptr ptr dd, ee : as integer ff
	as T2 ptr x : as T2 ptr ptr ptr ptr y : as T2 z
end type

type T : as integer   a,   b,   c : end type
type T : as integer  ptr a : as integer   b,   c : end type
type T : as integer ptr ptr a : as integer   b,   c : end type

type T : as integer   a : as integer  ptr b : as integer   c : end type
type T : as integer  ptr a,  b : as integer   c : end type
type T : as integer ptr ptr a : as integer  ptr b : as integer   c : end type

type T : as integer   a : as integer ptr ptr b : as integer   c : end type
type T : as integer  ptr a : as integer ptr ptr b : as integer   c : end type
type T : as integer ptr ptr a, b : as integer   c : end type

type T : as integer   a,   b : as integer  ptr c : end type
type T : as integer  ptr a : as integer   b : as integer  ptr c : end type
type T : as integer ptr ptr a : as integer   b : as integer  ptr c : end type

type T : as integer   a : as integer  ptr b,  c : end type
type T : as integer  ptr a,  b,  c : end type
type T : as integer ptr ptr a : as integer  ptr b,  c : end type

type T : as integer   a : as integer ptr ptr b : as integer  ptr c : end type
type T : as integer  ptr a : as integer ptr ptr b : as integer  ptr c : end type
type T : as integer ptr ptr a, b : as integer  ptr c : end type

type T : as integer   a,   b : as integer ptr ptr c : end type
type T : as integer  ptr a : as integer   b : as integer ptr ptr c : end type
type T : as integer ptr ptr a : as integer   b : as integer ptr ptr c : end type

type T : as integer   a : as integer  ptr b : as integer ptr ptr c : end type
type T : as integer  ptr a,  b : as integer ptr ptr c : end type
type T : as integer ptr ptr a : as integer  ptr b : as integer ptr ptr c : end type

type T : as integer   a : as integer ptr ptr b, c : end type
type T : as integer  ptr a : as integer ptr ptr b, c : end type
type T : as integer ptr ptr a, b, c : end type
