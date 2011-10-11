typedef unsigned __int8 uint8_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int64 uint64_t;

// type TT1 as T1
// (typedef needed, since it's a different id)
typedef struct T1 TT1;

// type typedef_T1 as T1
// (typedef not needed, since any places using T1 will work anyways)
typedef struct T1 T1;

typedef int *PINT;

typedef struct T1  /*boo*/ *  *  *  *  TT1;
