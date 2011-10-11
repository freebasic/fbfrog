/' TODO: token 105 '/ /' TODO: token 108 '/ __int8 uint8_t/' TODO: token 44 '/
/' TODO: token 105 '/ /' TODO: token 108 '/ __int32 uint32_t/' TODO: token 44 '/
/' TODO: token 105 '/ /' TODO: token 108 '/ __int64 uint64_t/' TODO: token 44 '/

'' type TT as T
'' (typedef needed, since it's a different id)
type TT as T

'' type typedef_T as T
'' (typedef not needed, since any places using T will work anyways)
type T as T

type PE as E ptr

type TT as U  /'boo'/ ptr  ptr  ptr  ptr
