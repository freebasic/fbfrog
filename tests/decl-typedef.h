typedef unsigned __int8 uint8_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int64 uint64_t;

// type TT as T
// (typedef needed, since it's a different id)
typedef struct T A;

typedef struct T (*A)(int i);

typedef T **A, B, (*C)(int);

typedef enum E *PE;

typedef union U /*boo*/ ****A;
