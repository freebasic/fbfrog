// @fbfrog -ifndefdecl NULL -ifndefdecl TRUE -ifndefdecl FALSE -ifndefdecl M1 -ifndefdecl M2 -ifndefdecl M3 -undefbeforedecl M3

#define NULL 0
#define TRUE 1
#define FALSE 0

#define M1 123

#undef M2
#define M2 123

#define M3 123
