__attribute__((warn_unused_result))     void f(void);
__attribute__((__warn_unused_result__)) void f(void);

__attribute__((noreturn))     void f(void);
__attribute__((__noreturn__)) void f(void);

__attribute__((malloc))     void f(void);
__attribute__((__malloc__)) void f(void);

__attribute__((deprecated))     void f(void);
__attribute__((__deprecated__)) void f(void);

__cdecl                    void f(void);
__attribute__((cdecl))     void f(void);
__attribute__((__cdecl__)) void f(void);

__stdcall                    void f(void);
__attribute__((stdcall))     void f(void);
__attribute__((__stdcall__)) void f(void);


static int before_base_type;
__attribute__((stdcall)) void f(void);

static int behind_base_type;
void __attribute__((stdcall)) f(void);

static int in_nested_declarator;
void (__attribute__((stdcall)) f)(void);

static int at_end_of_declarator;
void f(void) __attribute__((stdcall));
void f(int i __attribute__((deprecated)) = 0);

static int before_extern;
__attribute__((stdcall)) extern void f(void);

static int between_extern_and_base_type;
extern __attribute__((stdcall)) void f(void);

static int in_middle_of_base_type;
const __attribute__((stdcall)) short f(void);

static int all_should_be_stdcall;
__attribute__((stdcall)) void f1(void), f2(void);
void __attribute__((stdcall)) f1(void), f2(void);

static int only_f1_should_be_stdcall;
void (__attribute__((stdcall)) f1)(void), f2(void);
void f1(void) __attribute__((stdcall)), f2(void);

static int only_f2_should_be_stdcall;
void f1(void), __attribute__((stdcall)) f2(void);
void f1(void), (__attribute__((stdcall)) f2)(void);
void f1(void), f2(void) __attribute__((stdcall));

static int all_stdcall_function_pointers;
extern void (__attribute__((stdcall)) *f1)(void);
extern void (* __attribute__((stdcall)) f1)(void);
extern void __attribute__((stdcall)) (*f1)(void);
extern __attribute__((stdcall)) void (*f1)(void);
extern void (*f1)(void) __attribute__((stdcall));

extern int separator1;
declare function f( byval a as long ) as sub stdcall( byval b as long );
void (__attribute__((stdcall)) * f(int a))(int b);

extern int separator2;
declare function f( byval a as long ) as sub stdcall( byval b as long );
void (* __attribute__((stdcall)) f(int a))(int b);

extern int separator3;
declare function f stdcall( byval a as long ) as sub( byval b as long );
__attribute__((stdcall)) void (*f(int a))(int b);

extern int separator4;
declare function f stdcall( byval a as long ) as sub( byval b as long );
void __attribute__((stdcall)) (*f(int a))(int b);

extern int separator5;
declare function f stdcall( byval a as long ) as sub( byval b as long );
void (*f(int a))(int b) __attribute__((stdcall));

extern int separator6;
declare function f stdcall( byval a as long ) as function( byval b as long ) as sub( byval c as long );
__attribute__((stdcall)) void (*(*f(int a))(int b))(int c);

extern int separator7;
declare function f stdcall( byval a as long ) as function( byval b as long ) as sub( byval c as long );
void __attribute__((stdcall)) (*(*f(int a))(int b))(int c);

extern int separator8;
declare function f stdcall( byval a as long ) as function( byval b as long ) as sub( byval c as long );
void (*(*f(int a))(int b))(int c) __attribute__((stdcall));

extern int separator9;
declare function f( byval a as long ) as function( byval b as long ) as sub stdcall( byval c as long );
void (__attribute__((stdcall)) *(*f(int a))(int b))(int c);

extern int separator10;
declare function f( byval a as long ) as function stdcall( byval b as long ) as sub( byval c as long );
void (*(__attribute__((stdcall)) *f(int a))(int b))(int c);

extern int separator11;
extern p as function stdcall( byval a as long ) as sub( byval b as long );
extern __attribute__((stdcall)) void (*(*p)(int a))(int b);
extern void (*(__attribute__((stdcall)) *p)(int a))(int b);
extern void (*(*p)(int a))(int b) __attribute__((stdcall));

extern int separator12;
extern p as function( byval a as long ) as sub stdcall( byval b as long );
extern void (__attribute__((stdcall)) *(*p)(int a))(int b);

extern int separator13;
declare sub f stdcall( );
void   (__attribute__((stdcall)) f)  (void);
void  ((__attribute__((stdcall)) f)) (void);
void (((__attribute__((stdcall)) f)))(void);

extern int separator14;
declare function f stdcall( ) as sub( );
void (*  (__attribute__((stdcall)) f)  (void))(void);
void (* ((__attribute__((stdcall)) f)) (void))(void);
void (*(((__attribute__((stdcall)) f)))(void))(void);

extern int separator15;
extern p as sub stdcall( );
extern void (  __attribute__((stdcall))    *p     )(void);
extern void (  __attribute__((stdcall))   (*p)    )(void);
extern void (  __attribute__((stdcall))  ((*p))   )(void);
extern void (  __attribute__((stdcall)) (((*p)))  )(void);
extern void ( (__attribute__((stdcall))    *p   ) )(void);
extern void ( (__attribute__((stdcall))   (*p)  ) )(void);
extern void ( (__attribute__((stdcall))  ((*p)) ) )(void);
extern void ( (__attribute__((stdcall)) (((*p)))) )(void);
extern void (((__attribute__((stdcall))    *p   )))(void);
extern void (((__attribute__((stdcall))   (*p)  )))(void);
extern void (((__attribute__((stdcall))  ((*p)) )))(void);
extern void (((__attribute__((stdcall)) (((*p))))))(void);
extern void (  __attribute__((stdcall))    *(p)     )(void);
extern void (  __attribute__((stdcall))   (*(p))    )(void);
extern void (  __attribute__((stdcall))  ((*(p)))   )(void);
extern void (  __attribute__((stdcall)) (((*(p))))  )(void);
extern void ( (__attribute__((stdcall))    *(p)   ) )(void);
extern void ( (__attribute__((stdcall))   (*(p))  ) )(void);
extern void ( (__attribute__((stdcall))  ((*(p))) ) )(void);
extern void ( (__attribute__((stdcall)) (((*(p))))) )(void);
extern void (((__attribute__((stdcall))    *(p)   )))(void);
extern void (((__attribute__((stdcall))   (*(p))  )))(void);
extern void (((__attribute__((stdcall))  ((*(p))) )))(void);
extern void (((__attribute__((stdcall)) (((*(p)))))))(void);
