#undef FALSE
#undef TRUE
#undef NULL
const NULL = cptr(any ptr, 0)
const FALSE = 0
const TRUE = -1

declare function min(byval a as integer, byval b as integer) as integer
declare function max(byval a as integer, byval b as integer) as integer
