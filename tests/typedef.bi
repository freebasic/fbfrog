/' TODO: token 105 '/ /' TODO: token 108 '/ __int8 uint8_t/' TODO: token 44 '/
/' TODO: token 105 '/ /' TODO: token 108 '/ __int32 uint32_t/' TODO: token 44 '/
/' TODO: token 105 '/ /' TODO: token 108 '/ __int64 uint64_t/' TODO: token 44 '/

'' type TT1 as T1
'' (typedef needed, since it's a different id)
type TT1 as T1

'' type typedef_T1 as T1
'' (typedef not needed, since any places using T1 will work anyways)
type T1 as T1

type PINT as integer ptr

type TT1 as T1  /'boo'/ ptr  ptr  ptr  ptr
