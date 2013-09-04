static int id_id;
#define EXPANDME1 void id##id(void);
EXPANDME1
#undef EXPANDME1

static int id_param;
#define EXPANDME1(param) void id##param(void);
EXPANDME1(arg)
EXPANDME1(aa)
EXPANDME1(b)
EXPANDME1()
#undef EXPANDME1

static int param_id;
#define EXPANDME1(param) void param##id(void);
EXPANDME1(arg)
EXPANDME1(aa)
EXPANDME1(b)
EXPANDME1()
#undef EXPANDME1

static int param1_param2;
#define EXPANDME1(param1, param2) void param1##param2(void);
EXPANDME1(arg1, arg2)
EXPANDME1(a, b)
EXPANDME1(, b)
EXPANDME1(a, )
#undef EXPANDME1

static int id1_param_id2;
#define EXPANDME1(param) void id1##param##id2(void);
EXPANDME1(arg)
#undef EXPANDME1

static int param_id1_id2;
#define EXPANDME1(param) void param##id1##id2(void);
EXPANDME1(arg)
#undef EXPANDME1

static int id1_id2_param;
#define EXPANDME1(param) void id1##id2##param(void);
EXPANDME1(arg)
#undef EXPANDME1

static int param1_id_param2;
#define EXPANDME1(param1, param2) void param1##id##param2(void);
EXPANDME1(arg1, arg2)
#undef EXPANDME1

static int param1_param2_param3;
#define EXPANDME1(param1, param2, param3) void param1##param2##param3(void);
EXPANDME1(a,b,c)
#undef EXPANDME1

static int merge_affects_only_first_or_last_token_of_an_arg;
#define EXPANDME1(param) typedef struct id##param;
EXPANDME1(UDT A)
#undef EXPANDME1
#define EXPANDME1(param) typedef struct param##id;
EXPANDME1(UDT A)
#undef EXPANDME1
#define EXPANDME1(param) typedef struct id1##param##id2;
EXPANDME1(UDT A)
#undef EXPANDME1

static int spacedid_param;
#define EXPANDME1(param) void spacedid ##param(void);
EXPANDME1(arg)
#undef EXPANDME1

static int id_spacedparam;
#define EXPANDME1(spacedparam) void id## spacedparam(void);
EXPANDME1(arg)
#undef EXPANDME1

static int spacedid_spacedparam;
#define EXPANDME1(spacedparam) void spacedid ## spacedparam(void);
EXPANDME1(arg)
#undef EXPANDME1

static int spacedid_spacedparam;
#define EXPANDME1(spacedparam) void spacedid        ##      spacedparam(void);
EXPANDME1(arg)
#undef EXPANDME1
