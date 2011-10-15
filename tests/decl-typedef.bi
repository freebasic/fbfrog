typedef unsigned dim shared as __int8 uint8_t
typedef unsigned dim shared as __int32 uint32_t
typedef unsigned dim shared as __int64 uint64_t

'' type TT as T
'' (typedef needed, since it's a different id)
type as T A

type as function(byval i as integer) as T A

type as T ptr ptr A : type as T B : type as function(byval as integer) as T C

type as E ptr PE

type as U /'boo'/ ptr ptr ptr ptr A
