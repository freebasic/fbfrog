typedef unsigned __int8 uint8_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int64 uint64_t;

// type TT as T
// (typedef needed, since it's a different id)
typedef struct T TT;

// type typedef_T as T
// (typedef not needed, since any places using T will work anyways)
typedef struct T T;

typedef enum FOO *PENUMFOO;

typedef struct T  /*boo*/ *  *  *  *  TT;
